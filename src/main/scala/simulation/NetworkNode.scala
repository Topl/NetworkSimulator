package simulation

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random
import cats.implicits._
import cats._

class NetworkNode(val id: NodeId, x: Int, y: Int, val forger: Boolean = false, val distanceDelta: Double = 0) {
  var state: NetworkNodeState = NetworkNodeState(x, y, forger = forger, distanceDelta = distanceDelta)
  def contains(block: Block): Boolean = state.blocks.contains(block)

  var blockUpdates: mutable.Map[String, Int] = mutable.HashMap[String, Int]().withDefaultValue(0)

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
        .andThen(closeReputationBasedHotConnection(slotId, config, rnd))
        .andThen(openRandomNewHotConnections(slotId, config, rnd))
        .andThen(removeDisabledConnections(slotId, config))
        .apply(state)

    updateNodeState(resState)
  }

  private def forgeBlock(slotId: SlotId, config: Config, rnd: Random)(state: NetworkNodeState): NetworkNodeState =
    if (state.forger && Math.abs(rnd.nextLong() % 100) < config.blockForgePercentage) {
      val value = rnd.nextPrintableChar()
      println(s"$id: forge block in slot $slotId with value $value")
      val newBlock = Block(slotId, state.blocks.size + 1, value)
      val newChain = state.blocks + newBlock
      val blockId = getPseudoBlockId(newChain)
      blockUpdates(blockId) = 1000 // TODO ignore self-forged blocks?
      state.copy(blocks = newChain, lastForgedBlock = Option((slotId, newBlock)))
    } else {
      state
    }

  // actions
  private def removeDisabledConnections(slotId: SlotId, config: Config)(state: NetworkNodeState): NetworkNodeState = {
    val cold = state.coldConnections.filter(_._2.state.enabled)
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
    val connectionsDelta = config.minimumHotConnections - state.hotConnections.size
    // println(s"conn delta = ${id}:${connectionsDelta}")

    openRandomHotConnectionRec(connectionsDelta, state)
  }

  private def openConnectionWithBestPerformance(
                                       slotId: SlotId,
                                       config: Config,
                                       random: Random,
                                       state: NetworkNodeState
                                     ): NetworkNodeState = {
    val availableHosts = state.coldConnections -- state.hotConnections.keys
    val availableHostsSize = availableHosts.size
    // println(s"${id}: avail size ${availableHostsSize}")
    if (availableHostsSize > 0) {
      val (remoteId, remoteNode) = availableHosts.toIndexedSeq.minBy(d => calculateDistance(this, d._2))
      // println(s"${id}:new hot is ${remoteId}")
      val remoteNodeConnection = processNewHotConnection(slotId, remoteId, remoteNode, config)
      state.copy(hotConnections = state.hotConnections + (remoteId -> remoteNodeConnection), coldConnections = state.coldConnections - remoteId)
    } else {
      state
    }
  }

  private def openRandomHotConnection(
    slotId: SlotId,
    config: Config,
    random: Random,
    state:  NetworkNodeState
  ): NetworkNodeState = {
    val availableHosts = state.coldConnections -- state.hotConnections.keys
    val availableHostsSize = availableHosts.size
    // println(s"${id}: avail size ${availableHostsSize}")
    if (availableHostsSize > 0) {
      val (remoteId, remoteNode) = availableHosts.toSeq(random.nextInt(availableHosts.size))
      // println(s"${id}:new hot is ${remoteId}")
      val remoteNodeConnection = processNewHotConnection(slotId, remoteId, remoteNode, config)
      state.copy(hotConnections = state.hotConnections + (remoteId -> remoteNodeConnection))
    } else { state }
  }

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
      state.hotConnections.toList.sortBy(_._2.blockReputation).takeRight(config.minimumHotConnectionsBlock).toMap
    val byPerformance =
      state.hotConnections.toList
        .sortBy(_._2.performanceReputation)
        .takeRight(config.minimumHotConnectionPerformance)
        .toMap
    val byNewest =
      state.hotConnections.toList.sortBy(_._2.newReputation).takeRight(config.minimumHotConnectionNew).toMap

    val toRemove =
      state.hotConnections.withFilter(_._2.reputation < config.closeHotConnectionThreshold).map(_._1).toSet
    // if (toRemove.nonEmpty) println(s"${id}: close connection to $toRemove")

    val newHot = (state.hotConnections -- toRemove) ++ byBlocks ++ byPerformance ++ byNewest
    state.copy(hotConnections = newHot)
  }

  // end no simple

  private def processNewHotConnection(
    slotId:            SlotId,
    remoteNodeId:      NodeId,
    remoteNetworkNode: NetworkNode,
    config:            Config
  ): RemoteNodeConnection = {
    // println(s"${id}: Open hot connection to ${remoteNodeId}")
    val updateSlot = slotId + config.fetchingHostSlotDelay
    val performanceReputation = DistanceQuality(calculateDistance(this, remoteNetworkNode), config).toReputation(config)
    val newConnectionReputation = config.reputationNewConnection
    val blockReputation = 0
    val remoteNodeConnection =
      RemoteNodeConnection(remoteNetworkNode, blockReputation, performanceReputation, newConnectionReputation)
    remoteNetworkNode.putUpdateForSlot(updateSlot, UpdateFromPeer(id, coldPeers = Map(id -> this)))

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
            .filter(d => state.hotConnections.contains(d._1))
            .map { case (nodeId, chain) =>
              val block = getPseudoBlockId(chain)
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

  private def fetchRemoteColdHostsFromAllHotConnections(slotId: SlotId, config: Config, rnd: Random): Unit =
    // println(s"${id}: Fetch cold connections ${state.coldConnections.keySet}")
    if (slotId % 10 == 0 || state.coldConnections.size < config.minimumColdConnections) {
      val slotForUpdate = slotId + config.fetchingHostSlotDelay
      state.hotConnections.foreach { case (id, remoteNode) =>
        val coldFromHot = remoteNode.node.state.hotConnections.view.mapValues(_.node).toMap - this.id
        val update = UpdateFromPeer(peer = id, coldPeers = coldFromHot)
        putUpdateForSlot(slotForUpdate, update)
      }
    }

  private def fetchColdHostFromBlockProviders(slotId: SlotId, config: Config, rnd: Random): Unit =
    // println(s"${id}: Fetch cold connections ${state.coldConnections.keySet}")
    if (slotId % 10 == 0 || state.coldConnections.size < config.minimumColdConnections) {
      val slotForUpdate = slotId + config.fetchingHostSlotDelay

      val halfColdSlots =
        state.coldConnections.zipWithIndex.filter { case (conn, index) => index % 2 == 0 }.map(_._1).toMap
      if (halfColdSlots.size < config.minimumColdConnections) {
        val newColdHosts =
          state.hotConnections.toList
            .filter(_._2.blockReputation >= config.reputation2BlockReputation)
            .sortBy(_._2.blockReputation)
            .take(config.minimumHotConnectionsBlock)
            .flatMap { case (id, connection) =>
              connection.node.state.hotConnections.map { case (id, conn) => (id, conn.node) }
            }
            .toMap
        val update = UpdateFromPeer(peer = id, coldPeers = (halfColdSlots ++ newColdHosts) - id)
        putUpdateForSlot(slotForUpdate, update)

      }

    }

  private def updateNodeState(newState: NetworkNodeState): UpdateSummary = {
    val oldState = state.copy()
    state = newState
    NetworkNodeState.getDiff(oldState, state)
  }
}
