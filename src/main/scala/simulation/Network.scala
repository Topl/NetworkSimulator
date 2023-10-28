package simulation

import scala.collection.mutable
import scala.util.Random

class Network {
  private var lastNodeId = -1L

  def getNextNodeId: Long = {
    lastNodeId = lastNodeId + 1
    lastNodeId
  }

  val nodes: mutable.Map[NodeId, NetworkNode] = mutable.Map.empty
  private val blockIdToMetaInfo: mutable.Map[Block, BlockMetaInfo] = mutable.Map.empty.withDefaultValue(BlockMetaInfo())
  var lastSlotId: SlotId = 0

  def getPropagation95Mean(skipSlotsUpTo: Long): (Long, Double) = {
    val propagations =
      blockIdToMetaInfo
        .filter(_._1.slot > skipSlotsUpTo)
        .filter(_._2.propagation95Delta.isDefined)
        .map(_._2.propagation95Delta.get.toDouble)
        .toSeq

    (propagations.size, propagations.sum / propagations.size)
  }

  def getPropagation75Mean(skipSlotsUpTo: Long): (Long, Double) = {
    val propagations =
      blockIdToMetaInfo
        .filter(_._1.slot > skipSlotsUpTo)
        .filter(_._2.propagation75Delta.isDefined)
        .map(_._2.propagation75Delta.get.toDouble)
        .toSeq

    (propagations.size, propagations.sum / propagations.size)
  }

  def addNode(x: Int, y: Int, forger: Boolean, distanceReducer: Double): NodeUpdate.AddNode = {
    val nodeId = getNextNodeId
    val node = new NetworkNode(nodeId, x, y, forger, distanceReducer)
    nodes.put(nodeId, node)
    NodeUpdate.AddNode(nodeId, node, x, y)
  }

  def addRandomNode(x: Int, y: Int, rnd: Random, forger: Boolean, distanceReducer: Double): NodeUpdate.AddNode = {


    addNode(x, y, forger, distanceReducer)
  }

  def removeRandomNode(config: Config, rnd: Random): Option[NodeUpdate.RemoveNode] = {
    val enabled = nodes.filter(d => d._2.state.enabled && d._1 != 0).toSeq
    if (enabled.size > 1) {
      removeNode(rnd.nextInt(enabled.size - 1) + 1)
    } else { None }
  }

  def removeNode(id: NodeId): Option[NodeUpdate.RemoveNode] =
    nodes.get(id).filter(_.state.enabled).map { node =>
      node.state = node.state.copy(enabled = false)
      NodeUpdate.RemoveNode(id)
    }

  def addColdPeerForNode(forNode: NodeId, knownNodes: Seq[NodeId]): NodeUpdate = {
    val toAdd = knownNodes.map(id => (id, RemoteConnection(nodes(id), 0, 0, 0))).toMap

    val node = nodes(forNode)
    val oldState = node.state.copy()
    node.state = node.state.copy(coldConnections = oldState.coldConnections ++ toAdd)
    NodeUpdate.ChangeNode(node.id, node, NetworkNodeState.getDiff(oldState, node.state))
  }

  def processSlot(slotId: SlotId, rnd: Random, config: Config): Seq[NodeUpdate] = {
    val changes: Seq[NodeUpdate.ChangeNode] = nodes
      .filter(_._2.state.enabled)
      .map { case (id, node) =>
        NodeUpdate.ChangeNode(id, node, node.processSlot(slotId, rnd, config))
      }
      .toSeq

    forgingBlockStatistic(slotId, changes)
    // println(blockIdToMetaInfo)
    lastSlotId = slotId

    changes
  }

  private def forgingBlockStatistic(slotId: SlotId, changes: Seq[NodeUpdate.ChangeNode]): Unit = {
    // val newForgedBlocks = changes.filter(_.updateSummary.forgedBlock.isDefined).map(_.updateSummary.forgedBlock.get)

    changes.foreach { update =>
      update.updateSummary.newBlocksSuffix.foreach { b =>
        val old = blockIdToMetaInfo(b)
        blockIdToMetaInfo(b) = old.copy(totalCount = old.totalCount + 1)
      }

      update.updateSummary.removedBlocksPrefix.foreach { b =>
        val old = blockIdToMetaInfo(b)
        blockIdToMetaInfo(b) = old.copy(totalCount = old.totalCount - 1)
      }
    }

    blockIdToMetaInfo.filter(_._2.totalCount <= 0).foreach(d => blockIdToMetaInfo.remove(d._1))

    val allNodesSize = nodes.count(_._2.state.enabled)
    val allAcceptedBlocks = changes.flatMap(_.updateSummary.newBlocksSuffix)

    val propagation95Threshold = allNodesSize * 0.95
    val allAccepted95Blocks =
      allAcceptedBlocks.filter { b =>
        val meta = blockIdToMetaInfo(b)
        meta.propagation95Delta.isEmpty && meta.totalCount >= propagation95Threshold
      }.toSet
    allAccepted95Blocks.foreach { b =>
      val old = blockIdToMetaInfo(b)
      val propagation = slotId - b.slot
      blockIdToMetaInfo(b) = old.copy(propagation95Delta = Option(propagation))
      // println(s"Fully accepted block ${b}, for ${slotId - b.slot} slots")}
    }

    val propagation75Threshold = allNodesSize * 0.75
    val allAccepted75Blocks =
      allAcceptedBlocks.filter { b =>
        val meta = blockIdToMetaInfo(b)
        meta.propagation75Delta.isEmpty && meta.totalCount >= propagation75Threshold
      }.toSet

    allAccepted75Blocks.foreach { b =>
      val old = blockIdToMetaInfo(b)
      val propagation = slotId - b.slot
      blockIdToMetaInfo(b) = old.copy(propagation75Delta = Option(propagation))
      // println(s"Fully accepted block ${b}, for ${slotId - b.slot} slots")}
    }
  }

  def getBestBlockSourcePercent(config: NetworkConfig): Seq[Double] = {
    val blocks = nodes.filter(d => d._2.state.enabled && d._1 != 0).head._2.state.blocks
    if (blocks.lastOption.map(_.slot).getOrElse(0L) < config.statisticSkipBlocksWithSlotLess) {
      Seq(0.0)
    } else {
      val actualBlocks = blocks.dropWhile(_.slot < config.statisticSkipBlocksWithSlotLess)
      val bestSourceSize =
        actualBlocks.groupBy(_.source).map { case (id, blocks) => blocks.size.toDouble / actualBlocks.size }
      bestSourceSize.toSeq
    }
  }
}

case class BlockMetaInfo(
  totalCount:         Int = 0,
  propagation75Delta: Option[Long] = None,
  propagation95Delta: Option[Long] = None
)
