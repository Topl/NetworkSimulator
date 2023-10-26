package simulation

import scala.collection.immutable.ListSet

case class NetworkNodeState(
  x:               Int,
  y:               Int,
  forger:          Boolean,
  enabled:         Boolean = true,
  blocks:          ListSet[Block] = ListSet.empty,
  hotConnections:  Map[NodeId, RemoteConnection] = Map.empty,
  warmConnections: Map[NodeId, RemoteConnection] = Map.empty,
  coldConnections: Map[NodeId, RemoteConnection] = Map.empty,
  distanceDelta:   Double = 0,
  lastForgedBlock: Option[(SlotId, Block)] = None
) {
  def getConnection(id: NodeId): Option[RemoteConnection] = {
    hotConnections.get(id).orElse(warmConnections.get(id)).orElse(coldConnections.get(id))
  }
}

object NetworkNodeState {

  def getDiff(oldState: NetworkNodeState, newState: NetworkNodeState): UpdateSummary = {
    val newBlocks: Option[ListSet[Block]] = blockOrdering.compare(oldState.blocks, newState.blocks) match {
      case 0          => None
      case r if r > 0 => throw new IllegalStateException("new block is worse than old")
      case r if r < 0 => Option(newState.blocks)
    }

    val xDelta: Int = newState.x - oldState.x
    val yDelta: Int = newState.y - oldState.y

    val updatedHotPeer = newState.hotConnections.filter { case (nodeId, connection) =>
      oldState.hotConnections.get(nodeId).contains(connection)
    }
    val removedHotPeer = oldState.hotConnections -- newState.hotConnections.keySet

    val updatedWarmPeer = newState.warmConnections.filter { case (nodeId, connection) =>
      oldState.warmConnections.get(nodeId).contains(connection)
    }
    val removedWarmPeer = oldState.warmConnections -- newState.warmConnections.keySet

    val updatedColdPeer = newState.coldConnections.filter { case (nodeId, connection) =>
      oldState.coldConnections.get(nodeId).contains(connection)
    }
    val removedColdPeer = oldState.coldConnections -- newState.coldConnections.keySet

    val updates =
      updatedHotPeer.map { case (id, conn) => (id, conn.node) } ++
      removedHotPeer.map { case (id, conn) => (id, conn.node) } ++
      updatedWarmPeer.map { case (id, conn) => (id, conn.node) } ++
      removedWarmPeer.map { case (id, conn) => (id, conn.node) } ++
      updatedColdPeer.map { case (id, conn) => (id, conn.node) } ++
      removedColdPeer.map { case (id, conn) => (id, conn.node) }

    val addedBlocks = newBlocks.map(_.dropWhile(oldState.blocks.contains)).getOrElse(ListSet.empty)
    val removedBlocks = newBlocks.map(nb => oldState.blocks.dropWhile(nb.contains)).getOrElse(ListSet.empty)

    val lastForgedBlock =
      Option.when(newState.lastForgedBlock != oldState.lastForgedBlock)(newState.lastForgedBlock).flatten

    UpdateSummary(
      newBlocks,
      xDelta,
      yDelta,
      lastForgedBlock,
      addedBlocks,
      removedBlocks,
      updates.toSeq
    )
  }
}

case class RemoteConnection(
  node:                  NetworkNode,
  blockReputation:       Double,
  performanceReputation: Double,
  newReputation:         Double,
  lastClosedTimestamps:  Seq[Long] = Seq.empty
) {}
