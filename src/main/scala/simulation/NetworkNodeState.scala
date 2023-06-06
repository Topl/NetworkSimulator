package simulation

import cats.data.Chain

import scala.collection.immutable.{ListMap, ListSet}

case class NetworkNodeState(
  x:               Int,
  y:               Int,
  forger:          Boolean,
  enabled:         Boolean = true,
  blocks:          ListSet[Block] = ListSet.empty,
  hotConnections:  Map[NodeId, RemoteHotNodeConnection] = Map.empty,
  warmConnections: Map[NodeId, RemoteWarmConnection] = Map.empty,
  coldConnections: Map[NodeId, RemoteColdConnection] = Map.empty,
  distanceDelta:   Double = 0,
  lastForgedBlock: Option[(SlotId, Block)] = None
)

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

case class RemoteHotNodeConnection(
  node:                  NetworkNode,
  blockReputation:       Double,
  performanceReputation: Double,
  newReputation:         Double
) {

  val reputation: Double = {
    val mean = (blockReputation + performanceReputation + newReputation) / 3

    val max = Math.max(Math.max(blockReputation, performanceReputation), newReputation)
    val weighted = (2 * max + mean) / 3

    val meanWithoutNew = (blockReputation + performanceReputation) / 2
    meanWithoutNew
  }
}

case class RemoteColdConnection(
  node:       NetworkNode,
  reputation: Double
)

case class RemoteWarmConnection(node: NetworkNode, performance: Double)