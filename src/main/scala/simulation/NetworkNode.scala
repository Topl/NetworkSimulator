package simulation

import cats._
import cats.implicits._

import scala.collection.mutable
import scala.util.Random

class NetworkNode(val id: NodeId, x: Int, y: Int, val forger: Boolean = false, val distanceDelta: Double = 0) {
  var state: NetworkNodeState = NetworkNodeState(x, y, forger = forger, distanceDelta = distanceDelta)
  def contains(block: Block): Boolean = state.blocks.contains(block)

  def totalReputation(config: Config): Double =
    if (state.hotConnections.nonEmpty) {
      state.hotConnections
        .map(_._2.reputation)
        .toIndexedSeq
        .sorted
        .takeRight(config.minimumHotConnections)
        .sum / config.minimumHotConnections.toDouble
    } else { 0 }

  // how often block had been seen
  var blockUpdates: mutable.Map[String, Int] = mutable.HashMap[String, Int]().withDefaultValue(0)
  blockUpdates(emptyBlockchain) = 1000 // no update for empty blocks, i.e. we saw empty blockchain 1000 times

  private val updatesFromPeers: mutable.Map[SlotId, List[UpdateFromPeer]] =
    mutable.Map.empty.withDefaultValue(List.empty)

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
        .andThen(updateWarmHost(slotId, config, rnd))
        .andThen(moveColdToWarm(slotId, config, rnd))
        .andThen(moveHotToCold(slotId, config, rnd))
        .andThen(moveWarmToHot(slotId, config, rnd))
        .andThen(reputationDecoy(slotId, config, rnd))
        .andThen(removeDisabledConnections(slotId, config))
        .apply(state)

    // println(s"${id}: hot connections - ${resState.hotConnections}")
    updateNodeState(resState)
  }

  private def fetchRemoteUpdates(slotId: SlotId, config: Config, rnd: Random): Unit =
    state.hotConnections.foreach { case (id, remoteNode) =>
      val distance = calculateDistance(this, remoteNode.node)
      val slotForBlock = slotId + Math.ceil(distance / config.distancePerSlot).toInt

      val neighbours =
        if (isWarmPeerUpdateSlot(slotId, config)) {
          val perfRep = calculatePerfReputation(distance, config)
          val blockRep = remoteNode.blockReputation

          remoteNode.node.state.hotConnections.map { case (_, connection) =>
            connection.copy(
              blockReputation = blockRep,
              performanceReputation = 0,
              lastClosedTimestamps = Seq.empty
            )
          }.toSeq
        } else {
          Seq.empty
        }

      val peerBlocks =
        if (remoteNode.node.state.blocks != state.blocks) Option(remoteNode.node.state.blocks) else None

      putUpdateForSlot(slotForBlock, UpdateFromPeer(peer = id, peerBlocks = peerBlocks, newKnowNodes = neighbours))
    }

  private def isWarmPeerUpdateSlot(slotId: SlotId, config: Config): Boolean =
    slotId % config.warmHostUpdateEveryNSlots == 0

  private def forgeBlock(slotId: SlotId, config: Config, rnd: Random)(state: NetworkNodeState): NetworkNodeState =
    if (state.forger && couldForgeInSlot(slotId, config, rnd)) {
      val value = rnd.nextPrintableChar()
      // println(s"$id: forge block in slot $slotId with value $value")
      val newBlock = Block(slotId, state.blocks.size + 1, value, id)
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

  private def processUpdates(slotId: SlotId, config: Config)(state: NetworkNodeState): NetworkNodeState =
    updatesFromPeers
      .remove(slotId)
      .map { updates =>
        val allBlocks = state.blocks :: updates.flatMap(_.peerBlocks)
        val bestBlocks = allBlocks.max

        // add new connection: no source - by established warm connection; with source - by getting neighbours
        val newConnections: Seq[(NodeId, RemoteConnection)] =
          updates
            .flatMap(_.newKnowNodes)
            .filter(connection => state.getConnection(connection.node.id).isEmpty) // ignore already existed node
            .map(connection => connection.node.id -> connection)

        val allColdConnections = state.coldConnections ++ newConnections

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
          newBlocks.groupMapReduce(_._1)(_._2)(Math.min).map { case (id, blockCount) =>
            val updatedRep = knownSourcesToReputation(config, blockCount)

            val hotConnection = state.hotConnections(id) // we already filtered no longer exist connections before
            (id, hotConnection.copy(blockReputation = Math.max(updatedRep, hotConnection.blockReputation)))
          }

        // println(s"${id}: got cold: ${allColdConnections.keySet}")
        val newHot = state.hotConnections ++ updatedHotConnections
        // println(s"$id: reput ${newHot}")

        state.copy(blocks = bestBlocks, coldConnections = allColdConnections, hotConnections = newHot)
      }
      .getOrElse(state)

  private def updateWarmHost(slotId: SlotId, config: Config, rnd: Random)(state: NetworkNodeState): NetworkNodeState = {
    val newWarmPeerCount = config.minimumWarmConnections
    val maximumWarmConnections = config.maximumWarmConnections
    val warmPeersSize = state.warmConnections.size

    if (isWarmPeerUpdateSlot(slotId, config) && warmPeersSize < maximumWarmConnections && newWarmPeerCount > 0) {
      val minimumWarmPeers = config.minimumWarmConnections
      val warmHostsByReputation =
        state.warmConnections
          .map(d => d._1 -> d._2.reputation)
          .toList
          .sortBy(_._2)
      val warmHostsSize = warmHostsByReputation.size

      val warmToCold: Seq[NodeId] = warmHostsByReputation.take((warmHostsSize - minimumWarmPeers) / 2).map(_._1)

      val (newColdDelta, newWarmPeers) = state.warmConnections.partition(d => warmToCold.contains(d._1))
      val newColdPeers = state.coldConnections ++ newColdDelta.map { case (id, connection) =>
        id -> connection.copy(lastClosedTimestamps = connection.lastClosedTimestamps :+ slotId)
      }

      state.copy(coldConnections = newColdPeers, warmConnections = newWarmPeers)

    } else {
      state
    }
  }

  private def moveColdToWarm(slotId: SlotId, config: Config, rnd: Random)(state: NetworkNodeState): NetworkNodeState = {
    // @TODO config.minimumWarmConnections -> config.maximumWarmConnections
    val lackWarmPeersCount = config.maximumWarmConnections - state.warmConnections.size

    val eligibleCold = state.coldConnections.filter { case (id, connection) =>
      connection.lastClosedTimestamps.lastOption.forall { closedInSlot =>
        val totalCloses = connection.lastClosedTimestamps.size
        val nonEligibleWindow = totalCloses * totalCloses * config.closeTimeoutFirstDelayInSlots
        (slotId - closedInSlot) > nonEligibleWindow
      }
    }

    val randomPeers = Random.shuffle(eligibleCold.keySet).take(lackWarmPeersCount)

    val reputationPeers =
      eligibleCold.toSeq
        .sortBy { case (nodeId, peer) => (peer.blockReputation + peer.performanceReputation) }
        .takeRight(lackWarmPeersCount)
        .map(_._1)
        .toSet

    val toWarm = Random.shuffle(randomPeers ++ reputationPeers).take(lackWarmPeersCount)

    val (newWarmDelta, newCold) =
      state.coldConnections.partition(d => toWarm.contains(d._1))

    // put this node to list of known peer for REMOTE peer
    newWarmDelta.foreach { case (nodeId, remoteConnection) =>
      val distance = calculateDistance(this, remoteConnection.node)
      val slotForNewPeer = slotId + Math.ceil(distance / config.distancePerSlot).toInt
      // TODO
      val newKnownNode = RemoteConnection(this, blockReputation = 0, performanceReputation = 0, newReputation = 0)
      remoteConnection.node.putUpdateForSlot(slotForNewPeer, UpdateFromPeer(id, newKnowNodes = Seq(newKnownNode)))
    }

    val warmDeltaWithDistance =
      newWarmDelta.map { case (id, remoteConnection) =>
        val perfRep = calculatePerfReputation(calculateDistance(this, remoteConnection.node), config)
        id -> remoteConnection.copy(performanceReputation = perfRep)
      }

    state.copy(coldConnections = newCold, warmConnections = state.warmConnections ++ warmDeltaWithDistance)
  }

  private def moveHotToCold(slotId: SlotId, config: Config, rnd: Random)(state: NetworkNodeState): NetworkNodeState = {
    val saveByPerformanceReputation =
      state.hotConnections.toSeq
        .map { case (id, connection) => id -> connection.performanceReputation }
        .sortBy(_._2)
        .takeRight(config.minimumPerformanceReputationPeers)
        .map(_._1)
        .toSet

    val saveByBlockProviding =
      state.hotConnections.toSeq
        .map { case (id, connection) => id -> connection.blockReputation }
        .sortBy(_._2)
        .takeRight(config.minimumBlockProvidingReputationPeers)
        .map(_._1)
        .toSet

    val saveByNovelty =
      state.hotConnections
        .map { case (id, connection) => id -> connection.newReputation }
        .filter(_._2 > 0)
        .keys
        .toSet

    val saveByOverallReputation =
      state.hotConnections
        .filter { case (id, connection) =>
          val totalRep = (connection.blockReputation + connection.performanceReputation) / 2

          totalRep >= config.minimumRequiredReputation
        }
        .keys
        .toSet

    val allKeptConnections =
      saveByNovelty ++ saveByBlockProviding ++ saveByPerformanceReputation ++ saveByOverallReputation

    val (newHot, hotDelta) = state.hotConnections.partition(d => allKeptConnections.contains(d._1))
    val deltaWithClose =
      hotDelta.map { case (id, connection) =>
        id -> connection.copy(
          lastClosedTimestamps = connection.lastClosedTimestamps :+ slotId,
          blockReputation = 0,
          performanceReputation = 0
        )
      }

    val newCold = state.coldConnections ++ deltaWithClose

    state.copy(hotConnections = newHot, coldConnections = newCold)
  }

  private def moveWarmToHot(slotId: SlotId, config: Config, rnd: Random)(state: NetworkNodeState): NetworkNodeState = {
    val lackByConfig = config.minimumHotConnections - state.hotConnections.size
    val lackByReputation = 0//if (isWarmPeerUpdateSlot(slotId, config)) 1 else 0 // if (totalReputation(config) < 0.85) 1 else 0

    val lackHotPeersCount = Math.max(lackByConfig, lackByReputation)
    val random = Random.shuffle(state.warmConnections.keys.take(lackHotPeersCount)).toSet

    val reputation = state.warmConnections
      .map { case (id, connection) =>
        val totalRep = (connection.blockReputation + connection.performanceReputation) / 2

        id -> totalRep
      }
      .toSeq
      .sortBy(_._2)
      .takeRight(lackHotPeersCount)
      .map(_._1)

    val newHotIds = Random.shuffle(random ++ reputation).take(lackHotPeersCount)

    val (newHotDelta, newWarm) = state.warmConnections.partition(d => newHotIds.contains(d._1))
    val newHotDeltaWithNewRepUpdate =
      newHotDelta.map { case (id, connection) =>
        id -> connection.copy(newReputation = config.remotePeerNoveltyInSlots)
      }

    state.copy(hotConnections = state.hotConnections ++ newHotDeltaWithNewRepUpdate, warmConnections = newWarm)
  }

  private def reputationDecoy(slotId: SlotId, config: Config, rnd: Random)(
    state: NetworkNodeState
  ): NetworkNodeState = {
    val newHotConnection =
      state.hotConnections.map { case (host, connection) =>
        val blockReputation = connection.blockReputation * config.blockNoveltyDecoy
        val newReputation = Math.max(0, connection.newReputation - 1)
        (host, connection.copy(blockReputation = blockReputation, newReputation = newReputation))
      }

    state.copy(hotConnections = newHotConnection)
  }

  private def updateNodeState(newState: NetworkNodeState): UpdateSummary = {
    val oldState = state.copy()
    state = newState
    NetworkNodeState.getDiff(oldState, state)
  }

  def knownSourcesToReputation(networkConfig: Config, knownSources: Long): Double =
    if (knownSources == 1) {
      1
    } else {
      val reputationReducing: Double = (knownSources - 1) * networkConfig.blockNoveltyReputationStep
      Math.max(networkConfig.blockNoveltyInitialValue - reputationReducing, 0)
    }
}
