import cats.kernel.Eq

import scala.collection.immutable.ListSet

package object simulation {
  type NodeId = Long
  type SlotId = Long

  case class Block(slot: Long, height: Long, blockValue: Char, source: NodeId)
  implicit val BlockEq: Eq[Block] = (x: Block, y: Block) => x == y

  case class UpdateFromPeer(
    peer:              NodeId,
    peerBlocks:        Option[ListSet[Block]] = None,
    newKnowNodes: Seq[RemoteConnection] = Seq.empty //source of known nodes, new nodes
  )

  case class UpdateSummary(
    newBlocks:           Option[ListSet[Block]] = None,
    xDelta:              Int = 0,
    yDelta:              Int = 0,
    forgedBlock:         Option[(SlotId, Block)] = None,
    newBlocksSuffix:     ListSet[Block] = ListSet.empty,
    removedBlocksPrefix: ListSet[Block] = ListSet.empty,
    updatedConnection:   Seq[(NodeId, NetworkNode)]
  )

  sealed abstract class NodeUpdate

  object NodeUpdate {
    case class AddNode(nodeId: NodeId, node: NetworkNode, x: Int, y: Int) extends NodeUpdate

    case class RemoveNode(nodeId: NodeId) extends NodeUpdate

    case class ChangeNode(nodeId: NodeId, node: NetworkNode, updateSummary: UpdateSummary) extends NodeUpdate

    object NoOp extends NodeUpdate
  }

  implicit val blockOrdering: Ordering[ListSet[Block]] =
    Ordering
      .by[ListSet[Block], Long](_.size)
      .orElseBy(_.lastOption.map(-_.slot))
      .orElseBy(_.lastOption.map(_.blockValue.toLong).getOrElse(0L))

  def calculateDistance(node1: NetworkNode, node2: NetworkNode): Long = {
    val actualDistance = calculateDistance(node1.state.x, node1.state.y, node2.state.x, node2.state.y)

    Math.max(1, Math.round(actualDistance + (node1.state.distanceDelta + node2.state.distanceDelta) / 2))
  }

  def calculateDistance(x1: Long, y1: Long, x2: Long, y2: Long): Double = {
    val x = math.abs(x1 - x2)
    val y = math.abs(y1 - y2)

    math.sqrt(x * x + y * y)
  }

  sealed abstract class DistanceQuality {
    def toReputation(config: Config): Double
  }

  def calculatePerfReputation(distance: Long, config: Config): Double = {
    1 - (distance / config.maxDistance)
  }

  object DistanceQuality {

    def apply(distance: Double, config: Config): DistanceQuality = {
      val distanceInSlot = distance / config.distancePerSlot

      if (distanceInSlot <= config.distanceInSlotClose) Close
      else if (distanceInSlot <= config.distanceInSlotNormal) {
        Normal
      } else if (distanceInSlot <= config.distanceInSlotFurther) Further
      else VeryFurther
    }

    object Close extends DistanceQuality {
      def toReputation(config: Config): Double = config.reputationDistanceClose
    }

    object Normal extends DistanceQuality {
      def toReputation(config: Config): Double = config.reputationDistanceNormal
    }

    object Further extends DistanceQuality {
      def toReputation(config: Config): Double = config.reputationDistanceFurther
    }

    object VeryFurther extends DistanceQuality {
      def toReputation(config: Config): Double = config.reputationDistanceVeryFurther
    }
  }

  val emptyBlockchain: String = ""

  def getLastPseudoBlockId(blocks: ListSet[Block]): String =
    blocks.lastOption.map(lastBlock => s"${blocks.size}:${lastBlock}").getOrElse(emptyBlockchain)
}
