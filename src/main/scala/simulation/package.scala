import cats.data.Chain
import cats.kernel.Eq

import scala.collection.immutable.ListSet

package object simulation {
  type NodeId = Long
  type SlotId = Long

  case class Block(slot: Long, height: Long, blockValue: Char)
  implicit val BlockEq: Eq[Block] = (x: Block, y: Block) => x == y

  case class UpdateFromPeer(
    peer:       NodeId,
    peerBlocks: Option[ListSet[Block]] = None,
    hotPeers:   Map[NodeId, RemoteNodeConnection] = Map.empty,
    warmPeers:  Map[NodeId, RemoteNodeConnection] = Map.empty,
    coldPeers:  Map[NodeId, NetworkNode] = Map.empty
  )

  case class UpdateSummary(
    newBlocks:      Option[ListSet[Block]] = None,
    xDelta:         Int = 0,
    yDelta:         Int = 0,
    forgedBlock: Option[(SlotId, Block)] = None,
    newBlocksSuffix: ListSet[Block] = ListSet.empty,
    removedBlocksPrefix: ListSet[Block] = ListSet.empty,
    updatedConnection: Seq[(NodeId, NetworkNode)]
  )

  sealed abstract class NodeUpdate

  object NodeUpdate {
    case class AddNode(nodeId: NodeId, node: NetworkNode, x: Int, y: Int) extends NodeUpdate

    case class RemoveNode(nodeId: NodeId) extends NodeUpdate

    case class ChangeNode(nodeId: NodeId, node: NetworkNode, updateSummary: UpdateSummary) extends NodeUpdate

    object NoOp extends NodeUpdate
  }

  case class RemoteNodeConnection(
    node:                  NetworkNode,
    blockReputation:       Double,
    performanceReputation: Double,
    newReputation:         Double
  ) {
    val reputation: Double ={
      val mean = (blockReputation * performanceReputation * newReputation) / 3
      val max = Math.max(Math.max(blockReputation, performanceReputation), newReputation)
      (2 * max + mean) / 3
    }
  }


  implicit val blockOrdering: Ordering[ListSet[Block]] =
    Ordering.by[ListSet[Block], Long](_.size).orElseBy(_.lastOption.map(_.blockValue.toLong).getOrElse(0L))

  def calculateDistance(node1: NetworkNode, node2: NetworkNode): Long = {
    val x = math.abs(node1.state.x - node2.state.x)
    val y = math.abs(node1.state.y - node2.state.y)

    val actualDistance = math.sqrt(x * x + y * y)

    Math.max(1, Math.round(actualDistance + (node1.state.distanceDelta + node2.state.distanceDelta) / 2))
  }

  sealed abstract class DistanceQuality {
    def toReputation(config: Config): Double
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

  def getPseudoBlockId(blocks: ListSet[Block]): String = s"${blocks.size}:${blocks.lastOption.getOrElse("")}"

}
