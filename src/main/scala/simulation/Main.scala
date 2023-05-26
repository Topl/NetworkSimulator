package simulation

import org.graphstream.graph.implementations.SingleGraph
import org.graphstream.ui.geom.{Point2, Point3}
import org.graphstream.ui.view.{View, Viewer}
import org.graphstream.ui.view.camera.DefaultCamera2D

import java.awt.event.{MouseEvent, MouseListener, MouseWheelEvent, MouseWheelListener}
import scala.swing.Component
import scala.util.Random

case class Config(
  seed:                               Long = 0,
  maxX:                               Int = 1000,
  maxY:                               Int = 1000,
  distancePerSlot:                    Int = 200,
  distanceInSlotClose:                Int = 1,
  distanceInSlotNormal:               Int = 3,
  distanceInSlotFurther:              Int = 5,
  fetchingHostSlotDelay:              Int = 1,
  blockForgePercentage:               Int = 10,
  minimumHotConnections:              Int = 10,
  minimumHotConnectionsBlock:         Int = 3,
  minimumHotConnectionPerformance:    Int = 3,
  minimumHotConnectionNew:            Int = 3,
  minimumColdConnections:             Int = 20,
  reputationNewConnection:            Double = 1,
  reputationNewDecoyPercentPerSlot:   Double = 0.1,
  reputation1BlockReputation:         Double = 1.0,
  reputation2BlockReputation:         Double = 0.8,
  reputation3BlockReputation:         Double = 0.6,
  reputation4BlockReputation:         Double = 0.4,
  reputationBlockDecoyPercentPerSlot: Double = 0.05,
  reputationDistanceClose:            Double = 1.0,
  reputationDistanceNormal:           Double = 0.75,
  reputationDistanceFurther:          Double = 0.5,
  reputationDistanceVeryFurther:      Double = 0.25,
  closeHotConnectionThreshold:        Double = 0.66
) {
  val maxDistance: Double = math.sqrt(maxX * maxX + maxY * maxY)
}

object Main {

  def main(args: Array[String]): Unit = {

    val config = Config()

    System.setProperty("org.graphstream.ui", "swing")
//    System.setProperty("org.graphstream.debug", "true");
//    System.setProperty("org.graphstream.ui", "javafx")
    // System.setProperty("gs.ui.renderer", "org.graphstream.ui.j2dviewer.J2DGraphRenderer")
    System.setProperty("sun.java2d.uiScale.enabled", "false")

    val graph: SingleGraph = new SingleGraph("Simple")

    graph.display(false)
    val view = new GraphRepresentation(graph, config)
    view.initDraw()

    val random = new Random(42)
    val network = new Network()
    val rootNode = network.addNode(100, 100, true, 0)

    view.updateGraph(Seq(rootNode))
    Thread.sleep(600)

    //val pipe = viewer.newViewerPipe()
    (0 to 2000).foreach { slotId =>
      if (slotId % 5 == 0) {
        val removedNode = network.removeRandomNode(config, random).getOrElse(NodeUpdate.NoOp)
        view.updateGraph(Seq(removedNode))
      }

      val updates = network.processSlot(slotId, random, config)
      view.updateGraph(updates)

      val newNode =
        if (network.nodes.count(_._2.state.enabled) < 1000) {
          // distance reducing is doubled Gaussian from 0 to max possible distance
          val distanceDelta = Math.max(Math.min(
            config.maxDistance,
            random.nextGaussian() * random.nextInt(Math.floor(config.maxDistance).toInt)
          ), -config.maxDistance)

          val newNode =
            network.addRandomNode(config, random, forger = slotId % 5 == 0, distanceDelta)
          val addKnown = network.addColdPeerForNode(newNode.nodeId, Seq(rootNode.nodeId))
          Seq(newNode, addKnown)
        } else Seq(NodeUpdate.NoOp)
      view.updateGraph(newNode)
      view.updateStatistic(network)
      //viewer.getDefaultView.getCamera.setBounds(0, 0, 0, 1000, 1000, 0)
      Thread.sleep(1000)
    }

//    graph.addNode("E")
//    graph.getNode("E").setAttribute("xy", 10, 10)
//    graph.getNode("E").setAttribute("ui.style", "fill-color: rgb(0,100,255);")

    // graph.display(true)
  }

  def drawBorders(graph: SingleGraph, config: Config): Unit = {
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
  }

  def showGraph(graph: SingleGraph, config: Config) = {
    drawBorders(graph, config)
    val viewer = graph.display(false)

    //
    // graph.setAttribute("ui.stylesheet", "sprite { shape: flow; size: 5px; z-index: 0; } sprite#S1 { fill-color: #373; } sprite#S2 { fill-color: #393; } sprite#S3 { fill-color: #3B3; }")
    viewer
  }
}
