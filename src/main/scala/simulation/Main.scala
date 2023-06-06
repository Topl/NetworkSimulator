package simulation

import org.graphstream.graph.implementations.SingleGraph

import scala.util.Random

case class Config(
  seed:                             Long = 0,
  maxX:                             Int = 1000,
  maxY:                             Int = 1000,
  distancePerSlot:                  Int = 200,
  fetchingHostSlotDelay:            Int = 1,
  warmConnectionsMaximum:           Int = 10,
  hotConnectionsMinimum:            Int = 5,
  hotConnectionsMinimumBlock:       Int = 2,
  hotConnectionMinimumPerformance:  Int = 2,
  coldConnectionsMinimum:           Int = 20,
  reputationMaximumNewConnection:   Int = 10,
  reputationForNewConnection:       Double = 1,
  reputationNewDecoyPercentPerSlot: Double = 0.1,
  reputation1BlockReputation:       Double = 1,
  reputation2BlockReputation:       Double = 0.8,
  reputation3BlockReputation:       Double = 0.6,
  reputation4BlockReputation:       Double = 0.4,
  // reputation for ideal block transmitter shall no go lower than reputation2BlockReputation,
  // thus we shall take into consideration forgingSlotsPerBlock, i.e.
  // reputation1BlockReputation * (1-reputationNewDecoyPercentPerSlot) * forgingSlotsPerBlock > reputation2BlockReputation
  reputationBlockDecoyPercentPerSlot: Double = 0.03,
  distanceInSlotClose:                Int = 1,
  distanceInSlotNormal:               Int = 3,
  distanceInSlotFurther:              Int = 5,
  reputationDistanceClose:            Double = 1.0,
  reputationDistanceNormal:           Double = 0.75, //0.75
  reputationDistanceFurther:          Double = 0.5, //0.5
  reputationDistanceVeryFurther:      Double = 0.25, //0.25
  closeHotConnectionThreshold:        Double = 0.66,
  forgingInitialPercent:              Int = 5,
  forgingGapWindowInSlots:            Int = 5,
  forgingProbabilityMultiplier:       Double = 1.5,
  forgingSlotsPerBlock:               Double = 6.5 // TODO calculate based on other values?
) {
  val maxDistance: Double = math.sqrt(maxX * maxX + maxY * maxY)

  val reputationNewBecameOldAfter: Double = forgingSlotsPerBlock * 2

  val minimumNewReputationThreshold: Double = {
    val decoy = Math.pow(1 - reputationNewDecoyPercentPerSlot, reputationNewBecameOldAfter)
    reputationForNewConnection * decoy
  }

  val coldConnectionFetchEveryNSlots:   Int = Math.round(forgingSlotsPerBlock * 4).toInt

}

case class NetworkConfig(
  config:                          Config,
  random:                          Random,
  totalSlots:                      Int = 2000,
  maximumNodes:                    Int = 500,
  createForgerEveryNSlots:         Int = 20,
  statisticSkipBlocksWithSlotLess: Long = 500,
  showGraph:                       Boolean = true
) {
  val maxDistanceChanger: Double = config.maxDistance / 5

  def distanceDelta: Double =
    Math.max(-maxDistanceChanger, Math.min(maxDistanceChanger, random.nextGaussian() * maxDistanceChanger))
  // val distanceDelta = 0
}

object Main {

  def main(args: Array[String]): Unit = {

    val random = new Random(42)
    val config = Config()
    val networkConfig = NetworkConfig(config, random)

    System.setProperty("org.graphstream.ui", "swing")
    System.setProperty("sun.java2d.uiScale.enabled", "false")

    val graph: SingleGraph = new SingleGraph("Simple")

    graph.display(false)
    val view = new GraphRepresentation(graph, config)
    view.initDraw()

    val network = new Network()
    val rootNode = network.addNode(100, 100, false, 0)

    view.updateGraph(Seq(rootNode))
    Thread.sleep(100)

    // val pipe = viewer.newViewerPipe()
    (0 to networkConfig.totalSlots).foreach { slotId =>
//      if (slotId % 5 == 0) {
//        val removedNode = network.removeRandomNode(config, random).getOrElse(NodeUpdate.NoOp)
//        view.updateGraph(Seq(removedNode))
//      }

      val updates = network.processSlot(slotId, random, config)
      if (networkConfig.showGraph) view.updateGraph(updates)

      val newNode =
        if (network.nodes.count(_._2.state.enabled) < networkConfig.maximumNodes) {
          val forger = slotId % networkConfig.createForgerEveryNSlots == 0
          val newNode =
            network.addRandomNode(config, random, forger = forger, networkConfig.distanceDelta)
          val addKnown = network.addColdPeerForNode(newNode.nodeId, Seq(rootNode.nodeId))
          Seq(newNode, addKnown)
        } else Seq(NodeUpdate.NoOp)
      if (networkConfig.showGraph) view.updateGraph(newNode)
      view.updateStatistic(network, networkConfig)
      Thread.sleep(100)
    }

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
}
