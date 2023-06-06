package simulation

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random
import cats.implicits._
import cats._

class NetworkNode(val id: NodeId, x: Int, y: Int, val forger: Boolean = false, val distanceDelta: Double = 0) {
  var state: NetworkNodeState = NetworkNodeState(x, y, forger = forger, distanceDelta = distanceDelta)
  def contains(block: Block): Boolean = state.blocks.contains(block)

  def totalReputation(config: Config): Double =
    if (state.hotConnections.nonEmpty) {
      state.hotConnections
        .map(_._2.reputation)
        .toIndexedSeq
        .sorted
        .takeRight(config.hotConnectionsMinimum)
        .sum / config.hotConnectionsMinimum.toDouble
    } else { 0 }

  var blockUpdates: mutable.Map[String, Int] = mutable.HashMap[String, Int]().withDefaultValue(0)
  blockUpdates(emptyBlockchain) = 1000 // no update for empty blocks

  private val updatesFromPeers: mutable.Map[SlotId, List[UpdateFromPeer]] = mutable.Map.empty

  def putUpdateForSlot(slotId: SlotId, updateFromPeer: UpdateFromPeer): Unit = {
    val updates = updatesFromPeers.getOrElse(slotId, List.empty)
    updatesFromPeers(slotId) = updateFromPeer :: updates
  }

  def processSlot(slotId: SlotId, rnd: Random, config: Config): UpdateSummary = {
    fetchRemoteUpdates(slotId, config, rnd)

    // states updates
    val resState =
      (forgeBlock(slotId, config, rnd) _)
        .andThen(removeDisabledConnections(slotId, config))
        .andThen(processUpdates(slotId, config))
        .andThen(warmIsCold(config, rnd))
        .andThen(closeReputationBasedHotConnection(slotId, config, rnd))
        .andThen(openRandomNewHotConnections(slotId, config, rnd))
        .andThen(removeDisabledConnections(slotId, config))
        .apply(state)

    // println(s"${id}: hot connections - ${resState.hotConnections}")
    updateNodeState(resState)
  }

  private def forgeBlock(slotId: SlotId, config: Config, rnd: Random)(state: NetworkNodeState): NetworkNodeState =
    // Math.abs(rnd.nextLong() % 100) < config.forgingInitialPercent) {
    if (state.forger && couldForgeInSlot(slotId, config, rnd)) {
      val value = rnd.nextPrintableChar()
      println(s"$id: forge block in slot $slotId with value $value")
      val newBlock = Block(slotId, state.blocks.size + 1, value)
      val newChain = state.blocks + newBlock
      val blockId = getLastPseudoBlockId(newChain)
      blockUpdates(blockId) = 1000 // TODO ignore self-forged blocks?
      state.copy(blocks = newChain, lastForgedBlock = Option((slotId, newBlock)))
    } else {
      state
    }

  private def couldForgeInSlot(slotId: SlotId, config: Config, rnd: Random) = {
    val lastBlockDelta = state.blocks.lastOption.map(slotId - _.slot).getOrElse(slotId).toInt
    val gapWindowDelta = lastBlockDelta - config.forgingGapWindowInSlots
    if (gapWindowDelta <= 0) {
      false
    } else {
      val probability = (config.forgingInitialPercent * gapWindowDelta * config.forgingProbabilityMultiplier)
      val forge = Math.abs(rnd.nextLong() % 100) < probability
      forge
    }
  }

  // actions
  private def removeDisabledConnections(slotId: SlotId, config: Config)(state: NetworkNodeState): NetworkNodeState = {
    val cold = state.coldConnections.filter(_._2.node.state.enabled)
    val warm = state.warmConnections.filter(_._2.node.state.enabled)
    val hot = state.hotConnections.filter(_._2.node.state.enabled)

    state.copy(coldConnections = cold, warmConnections = warm, hotConnections = hot)
  }

  // Simple solution, just open random hot connection
  private def openRandomNewHotConnections(slotId: SlotId, config: Config, random: Random)(
    state: NetworkNodeState
  ): NetworkNodeState = {

    @tailrec
    def openRandomHotConnectionRec(count: Int, currentState: NetworkNodeState): NetworkNodeState =
      if (count <= 0) {
        currentState
      } else {
        openRandomHotConnectionRec(count - 1, openConnectionWithBestPerformance(slotId, config, random, currentState))
      }

    // println(s"$id: hot connections size: ${state.hotConnections.size}")
    // val connectionsDelta = config.minimumHotConnections - state.hotConnections.size
    // println(s"conn delta = ${id}:${connectionsDelta}")
    val requiredNewConnections =
      Math.round(config.reputationMaximumNewConnection.toDouble * (1 - totalReputation(config)))
    val actualNewConnections = state.hotConnections.count(_._2.reputation >= config.closeHotConnectionThreshold)

    val toOpenCount = Math.max(0, requiredNewConnections - actualNewConnections)
    // println(s"${id}: required new: ${toOpenCount}")

    openRandomHotConnectionRec(toOpenCount.toInt, state)
  }

  private def openConnectionWithBestPerformance(
    slotId: SlotId,
    config: Config,
    random: Random,
    state:  NetworkNodeState
  ): NetworkNodeState = {
    val availableHosts = state.warmConnections -- state.hotConnections.keys
    val availableHostsSize = availableHosts.size
    // println(s"${id}: avail size ${availableHostsSize}")
    if (availableHostsSize > 0) {
      val (remoteId, remoteColdConnection) = availableHosts.toIndexedSeq.minBy(d => calculateDistance(this, d._2.node))
      // println(s"${id}:new hot is ${remoteId}")
      val remoteNodeConnection = processNewHotConnection(slotId, remoteId, remoteColdConnection.node, config)
      state.copy(
        hotConnections = state.hotConnections + (remoteId -> remoteNodeConnection),
        coldConnections = state.coldConnections - remoteId,
        warmConnections = state.warmConnections - remoteId
      )
    } else {
      state
    }
  }

//  private def openRandomHotConnection(
//    slotId: SlotId,
//    config: Config,
//    random: Random,
//    state:  NetworkNodeState
//  ): NetworkNodeState = {
//    val availableHosts = state.coldConnections -- state.hotConnections.keys
//    val availableHostsSize = availableHosts.size
//    // println(s"${id}: avail size ${availableHostsSize}")
//    if (availableHostsSize > 0) {
//      val (remoteId, remoteNode) = availableHosts.toSeq(random.nextInt(availableHosts.size))
//      // println(s"${id}:new hot is ${remoteId}")
//      val remoteNodeConnection = processNewHotConnection(slotId, remoteId, remoteNode, config)
//      state.copy(hotConnections = state.hotConnections + (remoteId -> remoteNodeConnection))
//    } else { state }
//  }

  // some reputation is involved
  private def closeReputationBasedHotConnection(slotId: SlotId, config: Config, rnd: Random)(
    state: NetworkNodeState
  ): NetworkNodeState =
    (reputationDecoy(slotId, config, rnd) _)
      .andThen(closeAllBadValueConnections(slotId, config, rnd))
      .apply(state)

  private def reputationDecoy(slotId: SlotId, config: Config, rnd: Random)(
    state: NetworkNodeState
  ): NetworkNodeState = {
    val hotConnections =
      state.hotConnections.view.mapValues { rn =>
        val updatedNewReputation = rn.newReputation * (1 - config.reputationNewDecoyPercentPerSlot)
        val updatedBlockReputation = rn.blockReputation * (1 - config.reputationBlockDecoyPercentPerSlot)
        rn.copy(newReputation = updatedNewReputation, blockReputation = updatedBlockReputation)
      }.toMap

    state.copy(hotConnections = hotConnections)
  }

  private def closeAllBadValueConnections(slotId: SlotId, config: Config, rnd: Random)(
    state: NetworkNodeState
  ): NetworkNodeState = {
    val byBlocks =
      state.hotConnections.toList.sortBy(_._2.blockReputation).takeRight(config.hotConnectionsMinimumBlock).toMap
    val byPerformance =
      state.hotConnections.toList
        .sortBy(_._2.performanceReputation)
        .takeRight(config.hotConnectionMinimumPerformance)
        .toMap

    val byNewest =
      state.hotConnections.toList
        .filter(_._2.newReputation > config.minimumNewReputationThreshold)
        .toMap

    // println(s"${id}: byNewest size: ${byNewest.size}")

    val toRemove =
      state.hotConnections.withFilter(_._2.reputation < config.closeHotConnectionThreshold).map(_._1).toSet
    // if (toRemove.nonEmpty) println(s"${id}: close connection to $toRemove")

    val preNewHot = (state.hotConnections -- toRemove) ++ byBlocks ++ byPerformance ++ byNewest
    val addHot = {
      val toAddCount = config.hotConnectionsMinimum - preNewHot.size
      (state.hotConnections -- preNewHot.keySet).toIndexedSeq.sortBy(_._2.reputation).takeRight(toAddCount).toMap
    }
    val newHot = preNewHot// ++ addHot


    // println(s"${id}: closed hot connection = ${state.hotConnections.size - newHot.size}")
    val closed = newHot -- state.hotConnections.keySet

    state.copy(hotConnections = newHot, coldConnections = state.coldConnections -- closed.keySet)
  }

  // end no simple

  private def processNewHotConnection(
    slotId:            SlotId,
    remoteNodeId:      NodeId,
    remoteNetworkNode: NetworkNode,
    config:            Config
  ): RemoteHotNodeConnection = {
    // println(s"${id}: Open hot connection to ${remoteNodeId}")
    val updateSlot = slotId + config.fetchingHostSlotDelay
    val performanceReputation = DistanceQuality(calculateDistance(this, remoteNetworkNode), config).toReputation(config)
    val newConnectionReputation = config.reputationForNewConnection
    val blockReputation = 0
    val remoteNodeConnection =
      RemoteHotNodeConnection(remoteNetworkNode, blockReputation, performanceReputation, newConnectionReputation)
    remoteNetworkNode.putUpdateForSlot(
      updateSlot,
      UpdateFromPeer(id, coldPeers = Map(id -> RemoteColdConnection(this, 0)))
    )

    remoteNodeConnection
  }

  private def processUpdates(slotId: SlotId, config: Config)(state: NetworkNodeState): NetworkNodeState =
    updatesFromPeers
      .remove(slotId)
      .map { updates =>
        val allBlocks = state.blocks :: updates.flatMap(_.peerBlocks)
        val bestBlocks = allBlocks.max

        val allColdConnections = state.coldConnections ++ updates.flatMap(_.coldPeers)

        // update hot connection block reputation
        val updateWithNewBlocks = updates.filter(_.peerBlocks.isDefined).map(u => (u.peer, u.peerBlocks.get))
        val newBlocks =
          updateWithNewBlocks
            .filter(d => state.hotConnections.contains(d._1)) // connection is still alive
            .map { case (nodeId, chain) =>
              val block = getLastPseudoBlockId(chain)
              val prevUpdates = blockUpdates(block)
              blockUpdates.put(block, prevUpdates + 1)
              // println(s"$id: block rep for node ${nodeId} with count ${prevUpdates + 1} for block ${block}")
              (nodeId, prevUpdates + 1)
            }

        val updatedHotConnections =
          newBlocks.map { case (id, blockCount) =>
            val updatedRep =
              blockCount match {
                case 1 => config.reputation1BlockReputation
                case 2 => config.reputation2BlockReputation
                case 3 => config.reputation3BlockReputation
                case 4 => config.reputation4BlockReputation
                case _ => 0
              }

            val hotConnection = state.hotConnections(id) // we already filtered no longer exist connections before
            (id, hotConnection.copy(blockReputation = Math.max(updatedRep, hotConnection.blockReputation)))
          }

        // println(s"${id}: got cold: ${allColdConnections.keySet}")
        val newHot = state.hotConnections ++ updatedHotConnections
        // println(s"$id: reput ${newHot}")

        state.copy(blocks = bestBlocks, coldConnections = allColdConnections, hotConnections = newHot)
      }
      .getOrElse(state)

  private def fetchRemoteUpdates(slotId: SlotId, config: Config, rnd: Random): Unit = {
    fetchRemoteBlocks(slotId, config)
    // fetchRemoteColdHostsFromAllHotConnections(slotId, config, rnd)
    fetchColdHostFromBlockProviders(slotId, config, rnd)
  }

  private def fetchRemoteBlocks(slotId: SlotId, config: Config): Unit =
    state.hotConnections.foreach { case (id, remoteNode) =>
      if (remoteNode.node.state.blocks != state.blocks) {
        val distance = calculateDistance(this, remoteNode.node)
        val slotForBlock = slotId + Math.ceil(distance / config.distancePerSlot).toInt
        putUpdateForSlot(slotForBlock, UpdateFromPeer(peer = id, peerBlocks = Option(remoteNode.node.state.blocks)))
      }
    }

//  private def fetchRemoteColdHosts(slotId: SlotId, config: Config, rnd: Random): Unit =
//    // println(s"${id}: Fetch cold connections ${state.coldConnections.keySet}")
//    if (state.coldConnections.size < config.minimumColdConnections) {
//      val slotForUpdate = slotId + config.fetchingHostSlotDelay
//      state.hotConnections.foreach { case (id, remoteNode) =>
//        val update = UpdateFromPeer(peer = id, coldPeers = remoteNode.node.state.coldConnections - this.id)
//        putUpdateForSlot(slotForUpdate, update)
//      }
//    }

//  private def fetchRemoteColdHostsFromAllHotConnections(slotId: SlotId, config: Config, rnd: Random): Unit =
//    // println(s"${id}: Fetch cold connections ${state.coldConnections.keySet}")
//    if (slotId % config.coldConnectionFetchEveryNSlots == 0 || state.coldConnections.size < config.coldConnectionsMinimum) {
//      val slotForUpdate = slotId + config.fetchingHostSlotDelay
//      state.hotConnections.foreach { case (id, remoteNode) =>
//        val coldFromHot = remoteNode.node.state.hotConnections.view.mapValues(_.node).toMap - this.id
//        val coldConnections = coldFromHot.map { case (id, node) => (id, RemoteColdConnection(node, 0)) }
//        val update = UpdateFromPeer(peer = id, coldPeers = coldConnections)
//        putUpdateForSlot(slotForUpdate, update)
//      }
//    }

  private def fetchColdHostFromBlockProviders(slotId: SlotId, config: Config, rnd: Random): Unit =
    // println(s"${id}: Fetch cold connections ${state.coldConnections.keySet}")
    if (
      slotId % config.coldConnectionFetchEveryNSlots == 0 || state.coldConnections.size < config.coldConnectionsMinimum
    ) {
      val slotForUpdate = slotId + config.fetchingHostSlotDelay

      val halfColdSlots =
        state.coldConnections.zipWithIndex.filter { case (conn, index) => index % 2 == 0 }.map(_._1).toMap
      if (halfColdSlots.size < config.coldConnectionsMinimum) {
        val coldHostFromRemoteHot =
          state.hotConnections.toList
            .filter(_._2.blockReputation >= config.reputation2BlockReputation)
            .sortBy(_._2.blockReputation)
            .take(config.hotConnectionsMinimumBlock)
            .flatMap { case (id, connection) =>
              connection.node.state.hotConnections.map { case (id, conn) => (id, conn.node) }
            }
            .toMap

        val newColdHosts: Map[NodeId, RemoteColdConnection] = if (coldHostFromRemoteHot.isEmpty) {
          state.hotConnections.flatMap(_._2.node.state.hotConnections.map { case (id, con) =>
            (id, RemoteColdConnection(con.node, 0))
          })
        } else {
          coldHostFromRemoteHot.map { case (id, node) => (id, RemoteColdConnection(node, 0)) }
        }

        val update = UpdateFromPeer(peer = id, coldPeers = (halfColdSlots ++ newColdHosts) - id)
        putUpdateForSlot(slotForUpdate, update)

      }

    }

  private def warmIsCold(config: Config, rnd: Random)(networkNodeState: NetworkNodeState): NetworkNodeState = {
    networkNodeState.copy(warmConnections =
        networkNodeState.coldConnections.map({case (id, connection) => (id, RemoteWarmConnection(connection.node, 0))}))
  }

  private def updateWarmConnectionsIfRequired(config: Config, rnd: Random)(networkNodeState: NetworkNodeState): NetworkNodeState = {
    val warmNoHot = networkNodeState.warmConnections -- networkNodeState.hotConnections.keySet

    if (warmNoHot.size.isEmpty) {
      val newWarm =
        Random.shuffle(networkNodeState.coldConnections)
          .take(config.warmConnectionsMaximum)
          .map{case (id, connection) => (id, RemoteWarmConnection(connection.node, calculateDistance(this, connection.node)))}
          .toMap

      val newCold = networkNodeState.coldConnections -- newWarm.keySet
      networkNodeState.copy(warmConnections = newWarm, coldConnections = newCold)
    } else {
      networkNodeState.copy(warmConnections = warmNoHot)
    }
  }

  private def updateNodeState(newState: NetworkNodeState): UpdateSummary = {
    val oldState = state.copy()
    state = newState
    NetworkNodeState.getDiff(oldState, state)
  }
}
