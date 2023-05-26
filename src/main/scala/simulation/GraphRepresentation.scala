package simulation

import org.graphstream.graph.implementations.SingleGraph

class GraphRepresentation(graph: SingleGraph, config: Config) {
  private val nodeTextSize = "32px"
  private val currentSlotId = "CurrentSlotId"
  private val totalNodes = "TotalNodes"
  private val totalForgers = "TotalForgers"
  private val blockPropagation95NodeId = "BlockPropagation95"
  private val blockPropagation75NodeId = "BlockPropagation75"


  def initDraw(): Unit = {
    graph.addNode("A")
    graph.getNode("A").setAttribute("xy", 0, 0)

    graph.addNode("B")
    graph.getNode("B").setAttribute("xy", config.maxX, 0)

    graph.addNode("C")
    graph.getNode("C").setAttribute("xy", config.maxX, config.maxY)

    graph.addNode("D")
    graph.getNode("D").setAttribute("xy", 0, config.maxY)

    graph.addEdge("AB", "A", "B")
    graph.addEdge("BC", "B", "C")
    graph.addEdge("CD", "C", "D")
    graph.addEdge("DA", "D", "A")

    val xShift = -600
    val yShift = -100
    val defaultUiStyle = s"text-alignment: right; text-size: ${nodeTextSize}; fill-color: rgb(255,255,255);"


    val currentSlotNode = graph.addNode(currentSlotId)
    currentSlotNode.setAttribute("xy", xShift, config.maxY)
    currentSlotNode.setAttribute("ui.style", defaultUiStyle)

    val totalNodesNode = graph.addNode(totalNodes)
    totalNodesNode.setAttribute("xy", xShift, config.maxY + 1 * yShift)
    totalNodesNode.setAttribute("ui.style", defaultUiStyle)

    val forgerNodesNode = graph.addNode(totalForgers)
    forgerNodesNode.setAttribute("xy", xShift, config.maxY + 2 * yShift)
    forgerNodesNode.setAttribute("ui.style", defaultUiStyle)

    val propagation95Node = graph.addNode(blockPropagation95NodeId)
    propagation95Node.setAttribute("xy", xShift, config.maxY + 3 * yShift)
    propagation95Node.setAttribute("ui.style", defaultUiStyle)

    val propagation75Node = graph.addNode(blockPropagation75NodeId)
    propagation75Node.setAttribute("xy", xShift, config.maxY + 4 * yShift)
    propagation75Node.setAttribute("ui.style", defaultUiStyle)
  }

  def updateStatistic(network: Network): Unit = {
    graph.getNode(currentSlotId).setAttribute("ui.label",
      s"Current slot id: ${network.lastSlotId}")
    graph.getNode(totalNodes).setAttribute("ui.label",
      s"Total nodes: ${network.nodes.count(_._2.state.enabled)}")
    graph.getNode(totalForgers).setAttribute("ui.label",
      s"Total forgers: ${network.nodes.count(n => n._2.state.enabled && n._2.forger)}")


    val (propagation95size, propagation95mean) = network.getPropagation95Mean
    graph.getNode(blockPropagation95NodeId).setAttribute("ui.label",
      f"Propagation mean time (95%%) for block: ${propagation95mean}%.2f, total blocks: $propagation95size")
    val (propagation75size, propagation75mean) = network.getPropagation75Mean
    graph.getNode(blockPropagation75NodeId).setAttribute("ui.label",
      f"Propagation mean time (75%%) for block: ${propagation75mean}%.2f, total blocks: $propagation75size")
  }

  def updateGraph(updates: Seq[NodeUpdate]): Unit =
    updates.foreach {
      case NodeUpdate.AddNode(nodeId, node, x, y)             => addNode(nodeId.toString, node, x, y)
      case NodeUpdate.RemoveNode(nodeId)                      => removeNode(nodeId.toString)
      case NodeUpdate.ChangeNode(nodeId, node, updateSummary) => changeNode(nodeId, node, updateSummary)
      case NodeUpdate.NoOp                                    => ()
    }

  def getNodeColor(node: NetworkNode): String =
    if (node.state.forger) s"rgb(0,255,0)" else s"rgb(0,0,255)"

  private def addNode(nodeId: String, node: NetworkNode, x: Int, y: Int): Unit = {
    graph.addNode(nodeId)
    graph.getNode(nodeId).setAttribute("xy", x, y)
    graph
      .getNode(nodeId)
      .setAttribute(
        "ui.style",
        s"fill-color: ${getNodeColor(node)}; shape: box; text-background-mode: plain; text-color: ${getNodeColor(node)};"
      )
  }

  private def removeNode(nodeId: String): Unit =
    graph.removeNode(nodeId)

  private def changeNode(thisNodeId: NodeId, node: NetworkNode, change: UpdateSummary): Unit = {
    change.newBlocks match {
      case Some(block) =>
        val lastBlock = block.lastOption.get
        val text = block.toList.takeRight(5).map(_.blockValue).mkString
        val red = lastBlock.blockValue.toByte
        val green = lastBlock.blockValue.toByte
        val blue = lastBlock.blockValue.toByte
        // graph.getNode(thisNodeId).setAttribute("ui.style", s"fill-color: rgb($red,$green,$blue);")
        graph.getNode(thisNodeId.toString).setAttribute("ui.style", s"text-alignment: above; text-size: ${nodeTextSize};")
        graph.getNode(thisNodeId.toString).setAttribute("ui.label", f"[${node.distanceDelta}%.1f]" + s"[${block.size}]$text")
      case None => ()
    }

    change.updatedConnection.foreach { case (remoteId, remoteNode) =>
      updateConnection(graph, thisNodeId, node, remoteId, remoteNode)
    }
  }

  private def updateConnection(
    graph:        SingleGraph,
    thisNodeId:   NodeId,
    thisNode:     NetworkNode,
    remoteNodeId: NodeId,
    remoteNode:   NetworkNode,
  ): Unit = {
    def distanceToColor(distanceQuality: DistanceQuality): String =
      distanceQuality match {
        case DistanceQuality.Close       => "rgb(0, 255, 0)"
        case DistanceQuality.Normal      => "rgb(0, 0, 0)"
        case DistanceQuality.Further     => "rgb(255, 165, 50)"
        case DistanceQuality.VeryFurther => "rgb(255, 0, 0)"
      }

    val thisNodeIdString = thisNodeId.toString
    val remoteNodeIdString = remoteNodeId.toString

    if (graph.getNode(thisNodeIdString) == null || graph.getNode(remoteNodeIdString) == null) return

    val distance = calculateDistance(thisNode, remoteNode)
    val color = distanceToColor(DistanceQuality(distance, config))
    val peerType: KnownPeer = KnownPeer.forNode(thisNode, remoteNode)
    val remoteKnownPeer: KnownPeer = KnownPeer.forNode(remoteNode, thisNode)
    val text = remoteKnownPeer.getEndSymbol.reverseEdgeLabel + "——" + peerType.getEndSymbol
    val (edgeId, label) =
      if (thisNodeId < remoteNodeId) {
        val firstRep = remoteNode.state.hotConnections.get(thisNodeId).map(c => f"${c.blockReputation}%.1f").getOrElse("")
        val secondRep = thisNode.state.hotConnections.get(remoteNode.id).map(c => f"${c.blockReputation}%.1f").getOrElse("")
        (s"$thisNodeId:$remoteNodeId", firstRep + text + secondRep)
      } else {
        val secondRep = remoteNode.state.hotConnections.get(thisNodeId).map(c => f"${c.blockReputation}%.1f").getOrElse("")
        val firstRep = thisNode.state.hotConnections.get(remoteNode.id).map(c => f"${c.blockReputation}%.1f").getOrElse("")
        (s"$remoteNodeId:$thisNodeId", firstRep + text.reverseEdgeLabel + secondRep)
      }

    if (peerType.show || remoteKnownPeer.show) {
      val edge =
        Option(graph.getEdge(edgeId))
          .getOrElse(graph.addEdge(edgeId, thisNodeIdString, remoteNodeIdString, false))

      edge.setAttribute("ui.style", s"text-alignment: along; text-size: 24px; text-style: bold; fill-color: ${color};")
      edge.setAttribute("ui.label", s"$label")
    } else {
      Option(graph.getEdge(edgeId)).map(graph.removeEdge)
    }
  }

  implicit class StringOps(s: String) {

    def reverseEdgeLabel: String =
      s.map {
        case '>' => '<'
        case '<' => '>'
        case c   => c
      }.reverse
  }
}

sealed abstract class KnownPeer {
  def getEndSymbol: String
  def show: Boolean
}

object KnownPeer {

  def forNode(thisNode: NetworkNode, remoteNode: NetworkNode): KnownPeer = {
    val remoteId = remoteNode.id
    if (thisNode.state.hotConnections.contains(remoteId)) HotPeer
    else if (thisNode.state.warmConnections.contains(remoteId)) WarmPeer
    else if (thisNode.state.coldConnections.contains(remoteId)) ColdPeer
    else NoPeer
  }

  object NoPeer extends KnownPeer {
    override def getEndSymbol: String = "  "
    override def show: Boolean = false
  }

  object ColdPeer extends KnownPeer {
    override def getEndSymbol: String = "++"
    override def show: Boolean = false
  }

  object WarmPeer extends KnownPeer {
    override def getEndSymbol: String = "+>"
    override def show: Boolean = false
  }

  object HotPeer extends KnownPeer {
    override def getEndSymbol: String = ">>"
    override def show: Boolean = true
  }
}