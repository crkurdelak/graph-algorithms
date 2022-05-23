import java.io.{File, IOException}
import java.util
import java.util.Scanner
import scala.collection.mutable
import scala.util.control.Exception
import scala.xml.XML.loadFile
import scala.xml.Node

object graph
{
	/*
	A trait for representing directed and undirected graphs
	*/
	trait Graph[T]
	{
		def isDirected:Boolean

		def getVertices:Iterable[T]

		def getEdge(source:T, destination:T):Option[Edge[T]]

		def getEdges():Iterable[Edge[T]]

		def edgeExists(source:T, destination:T):Boolean

		def getEdgeWeight(source:T, destination:T):Option[Int]

		@throws(classOf[IllegalArgumentException])
		def addVertex(vertex:T):Graph[T]

		@throws(classOf[IllegalArgumentException])
		def removeVertex(vertex:T):Graph[T]

		@throws(classOf[IllegalArgumentException])
		def addEdge(source:T, destination:T, weight:Int):Graph[T]

		@throws(classOf[IllegalArgumentException])
		def removeEdge(source:T, destination:T):Graph[T]

		@throws(classOf[IllegalArgumentException])
		def getAdjacent(source:T):Iterable[T]

		def pathLength(path:Seq[T]):Option[Long]

		@throws(classOf[IllegalArgumentException])
		def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]]

		def minimumSpanningTree:Option[Graph[T]]

		def greedyTSP():Seq[Edge[T]]

		def greedyTSP(initialTour:Seq[T]):Seq[Edge[T]]

		def branchBoundTSP:Seq[Edge[T]]

		def branchBoundTSP(heur:(Graph[T], Seq[T]) => Long):Seq[Edge[T]]

		def toString:String
	}


	/**
	 *  Serves as a factory function for producing new empty Graphs
	 */
	object Graph {
		/**
		 * Creates and returns a new empty Graph - acts as a constructor
		 *
		 * @param isDirected true for a directed graph, false for a non-directed graph
		 * @tparam T the type stored in the vertices
		 * @return an empty graph
		 */
		def apply[T](isDirected: Boolean): Graph[T] = {
			new GraphImpl[T](isDirected, Map())
		}


		/**
		 * Creates a new graph from a CSV file.
		 *
		 * @param isDirected true for a directed graph, false for a non-directed graph
		 * @param fileName the filename of the CSV file
		 * @throws java.io.IOException if there is an error while processing the input stream
		 * @return a new graph from the given CSV file
		 */
		@throws(classOf[IOException])
		def fromCSVFile(isDirected:Boolean, fileName:String):Graph[String] = {
			try {
				val sc: Scanner = new Scanner(new File(fileName))
				var vertices = List[String]()
				var edges = List[Edge[String]]()

				while (sc.hasNext()) {
					val numVertices: Int = sc.nextInt()
					sc.nextLine()
					// number of vertices = first line
					while (!sc.hasNextInt()) {
						vertices = vertices :+ sc.nextLine()
					}
					// until you see an int, make each line a vertex
					val numEdges = sc.nextInt()
					// number of edges = that int
					sc.nextLine()
					// until end of file, each line is src, dest, weight of an edge
					while (sc.hasNextLine()) {
						val line = sc.nextLine()
						val tokens = line.split(",")
						edges = edges :+ new Edge[String](tokens(0), tokens(1), tokens(2).toInt)
					}
				}

				var newGraph = Graph[String](false)
				for (vertex <- vertices) {
					newGraph = newGraph.addVertex(vertex)
				}
				for (edge <- edges) {
					newGraph = newGraph.addEdge(edge.source, edge.destination, edge.weight)
				}
				newGraph
			}
			catch {
				case e: Exception => throw new IOException()
			}
		}


		/**
		 * A private implementation of the Graph trait
		 *
		 * @param isDirected true for a directed graph, false for a non-directed graph
		 * @param _adjacencyMap the adjacency map of this graph
		 * @tparam T the type stored in the vertices
		 */
		private class GraphImpl[T](val isDirected: Boolean, val _adjacencyMap: Map[T, Set[(T, Int)]])
			extends Graph[T] {

			/**
			 * Returns a collection of the vertices of this graph.
			 *
			 * @return a collection of the vertices in this graph
			 */
			def getVertices: Iterable[T] = _adjacencyMap.keys


			/**
			 * Returns the edge from the given source to the given destination.
			 * @param source the source of the edge
			 * @param destination the destination of the edge
			 * @return the edge from the source to the destination, if such an edge exists, else return
			 *         None
			 */
			def getEdge(source:T, destination:T):Option[Edge[T]] = {
				var edge:Option[Edge[T]] = None
				if (edgeExists(source, destination)) {
					edge = Some(new Edge[T](source, destination, getEdgeWeight(source, destination).get))
				}
				edge
			}


			/**
			 * Returns a collection of all the edges in this graph.
			 * @return a collection of all the edges in this graph
			 */
			def getEdges():Iterable[Edge[T]] = {
				val vertices:Iterable[T] = getVertices
				var edges:List[Edge[T]] = List[Edge[T]]()

				for (vertex:T <- vertices) {
					for (other: T <- getAdjacent(vertex)) {
						val newEdge = getEdge(vertex, other).get
						if (isDirected) {
							if (!edges.contains(newEdge)) {
								edges = edges :+ newEdge
							}
						}
						else {
							// undirected graph has only one edge between each pair of connected vertices
							val otherEdge = getEdge(other, vertex).get
							val containsOther = edges.contains(otherEdge)
							if (!edges.contains(newEdge) && !(edges.contains(otherEdge))) {
								edges = edges :+ newEdge
							}
						}
					}
				}
				edges
			}


			/**
			 * Returns true if there is an edge from the given source to the given destination; else
			 * returns false.
			 *
			 * @param source the label of the source of the edge
			 * @param destination the label of the destination of the edge
			 * @return true if there is an edge from the given source to the given destination; else
			 * returns false
			 */
			def edgeExists(source: T, destination: T): Boolean = _adjacencyMap.getOrElse(source, Set())
				.exists(tuple => tuple._1 == destination)


			/**
			 * Returns the weight of the edge from the given source to the given destination.
			 *
			 * @param source the source of the edge
			 * @param destination the label of the destination of the edge
			 * @return the weight of the edge from the given source to the given destination
			 */
			def getEdgeWeight(source: T, destination: T): Option[Int] = {
				val destinations = _adjacencyMap.getOrElse(source, Set())
				val correctDest = destinations.find(tuple => tuple._1 == destination)
				correctDest.map(tuple => tuple._2)
			}


			/**
			 * Adds a new vertex to this graph.
			 *
			 * @param vertex the label of the new vertex
			 * @return the graph with the given vertex added
			 */
			def addVertex(vertex: T): Graph[T] = {
				if (!_adjacencyMap.contains(vertex) && vertex != null) {
					val updatedAdj: Map[T, Set[(T, Int)]] = _adjacencyMap + (vertex -> Set())
					new GraphImpl(isDirected, updatedAdj)
				}
				else throw new IllegalArgumentException
			}


			/**
			 * Removes the specified vertex from this graph.
			 *
			 * @param vertex the vertex to remove
			 * @return the graph with the given vertex removed
			 */
			def removeVertex(vertex: T): Graph[T] = {
				if (_adjacencyMap.contains(vertex)) {
					var updatedAdj = _adjacencyMap.removed(vertex)
					var edges:Set[(T, Int)] = Set[(T, Int)]()
					for (key:T <- updatedAdj.keys) {
						edges = updatedAdj(key).filter(pair => pair._1 != vertex)
						updatedAdj = updatedAdj + (key -> edges)
						edges = Set[(T, Int)]()
					}
					new GraphImpl(isDirected, updatedAdj)
				}
				else throw new IllegalArgumentException
			}


			/**
			 * Adds a new edge to this graph.
			 *
			 * @param source the label of the source of the new edge
			 * @param destination the label of the destination of the new edge
			 * @param weight the weight of the new edge
			 * @return the graph with the given edge added
			 */
			def addEdge(source: T, destination: T, weight: Int): Graph[T] = {
				if (this._adjacencyMap.contains(source) && this._adjacencyMap.contains(destination)
					&& !edgeExists(source, destination)	&& source != destination) {
					var updatedSet: Set[(T, Int)] = _adjacencyMap(source) + ((destination, weight))
					var updatedMap = _adjacencyMap + (source -> updatedSet)
					if (!isDirected) {
						updatedSet = _adjacencyMap(destination) + ((source, weight))
						updatedMap = updatedMap + (destination -> updatedSet)
					}

					new GraphImpl(isDirected, updatedMap)
				}
				else throw new IllegalArgumentException
			}


			/**
			 * Removes the specified edge from this graph.
			 *
			 * @param source      the source of the edge to remove
			 * @param destination the destination of the edge to remove
			 * @return this graph with the specified edge removed
			 */
			def removeEdge(source: T, destination: T): Graph[T] = {
				if (edgeExists(source, destination)) {
					var edges = _adjacencyMap(source).filter(pair => pair._1 != destination)

					var updatedAdj: Map[T, Set[(T, Int)]] = _adjacencyMap + (source -> edges)

					if (!isDirected) {
						edges = _adjacencyMap(destination).filter(pair => pair._1 != source)

						updatedAdj = updatedAdj + (destination -> edges)
					}
					new GraphImpl(isDirected, updatedAdj)
				}
				else throw new IllegalArgumentException
			}


			/**
			 * Returns all the vertices adjacent to this vertex.
			 *
			 * Adjacent vertices are connected by an edge.
			 *
			 * @param source the label of the source vertex
			 * @throws java.lang.IllegalArgumentException if the argument is null or the given source
			 *                                            does not exist
			 * @return a collection of all the vertices adjacent to the given source
			 */
			@throws(classOf[IllegalArgumentException])
			def getAdjacent(source:T):Iterable[T] = {
				if (source != null && _adjacencyMap.contains(source)) {
					for (tuple <- _adjacencyMap(source)) yield {
						tuple._1
					}
				}
				else throw new IllegalArgumentException
			}


			/**
			 * Returns the length of the given path.
			 *
			 * @param path a path represented as a sequence of vertices
			 * @return the length of the given path
			 */
			def pathLength(path:Seq[T]):Option[Long] = {
				var output:Option[Long] = Some(0)
				if (path.size > 1) {
					var currentLength: Long = 0

					for (pair <- path.sliding(2)) {
						val start = pair.head
						val end = pair.tail.head
						val weight = getEdgeWeight(start, end)
						if (weight.isDefined) {
							currentLength = currentLength + weight.get
						}
						else {
							output = None
						}
					}
					if (output.isDefined)
						output = Some(currentLength)
				}
				else {
					output = None
				}
				output
			}


			/**
			 * Returns the shortest path between two vertices.
			 *
			 * Uses Dijkstra's Algorithm
			 *
			 * @param source the label of the source vertex
			 * @param destination the label of the destination vertex
			 * @throws java.lang.IllegalArgumentException if any of the given vertices do not exist
			 * @return the shortest path between the source and destination vertices
			 */
			@throws(classOf[IllegalArgumentException])
			def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]] = {
				if (source != null && destination != null && _adjacencyMap.contains(source) &&
					_adjacencyMap.contains(destination)) {

					var dist:Map[T, Int] = Map()
					var parent:Map[T, T] = Map()
					var visited:Set[T] = Set()

					dist = dist + (source -> 0)
					parent = parent + (source -> source)

					var current = source
					var stillConnected = true
					var destinationFound = false

					var unvisited = dist.filter(pair => !visited.contains(pair._1))
					if (unvisited.isEmpty) {
						stillConnected = false
					}
					while (visited.size < this.getVertices.size && stillConnected && !destinationFound)
					{
						unvisited = dist.filter(pair => !visited.contains(pair._1))
						if (unvisited.nonEmpty)
						{
							current = unvisited.minBy(pair => pair._2)._1
						}
						else {
							stillConnected = false
						}
						if (current == destination) {
							destinationFound = true
						}
						visited = visited + current

						if (stillConnected) {
							for (other <- this.getAdjacent(current)) {
								if (!visited.contains(other)) {
									val newDist: Int = this.getEdgeWeight(current, other).get + dist(current)

									if (!dist.contains(other) || newDist < dist(other)) {
										dist = dist + (other -> newDist)
										parent = parent + (other -> current)
									}
								}
							}
						}
					}

					if (stillConnected) {
						var edges: Seq[Edge[T]] = Seq()

						// start at end, get parent until something is its own parent
						var vertex = destination
						while (!vertex.equals(parent(vertex))) {
							val edge: Edge[T] = new Edge(parent(vertex), vertex, getEdgeWeight(parent
							(vertex), vertex).get)
							edges = edges :+ edge
							vertex = parent(vertex)
						}

						Some(edges)
					}
					else {
						None
					}
				}
				else throw new IllegalArgumentException
			}


			/**
			 * Finds a minimum spanning tree of this graph using Prim's algorithm.
			 *
			 * It is illegal to ask for the minimum spanning tree of a directed graph.
			 *
			 * @return a minimum spanning tree of this graph if one exists, else returns None
			 */
			def minimumSpanningTree:Option[Graph[T]] = {
				var tree:Graph[T] = null
				var stillConnected = true
				if (getVertices.nonEmpty) {
					if (!isDirected) {
						var dist: Map[T, Int] = Map()
						var parent: Map[T, T] = Map()
						var visited: Set[T] = Set()
						// this will be the minimum spanning tree
						tree = Graph(false)

						val allVertices: Seq[T] = getVertices.toSeq
						for (vertex <- allVertices) {
							tree = tree.addVertex(vertex)
						}

						//start = pick arbitrary vertex
						val start: T = allVertices.head
						//Initialize parent and dist with vertices adjacent to start
						val adj = getAdjacent(start)
						for (vertex <- adj) {
							dist = dist + (vertex -> getEdgeWeight(start, vertex).get)
							parent = parent + (vertex -> start)
						}
						var current = start

						var unvisited = dist.filter(pair => !visited.contains(pair._1))
						if (unvisited.isEmpty) {
							stillConnected = false
						}
						while (visited.size < this.getVertices.size && stillConnected) {
							//current = find the closest, unvisited vertex in dist
							unvisited = dist.filter(pair => !visited.contains(pair._1))
							if (unvisited.nonEmpty) {
								current = unvisited.minBy(pair => pair._2)._1
							}
							else {
								stillConnected = false
							}
							visited = visited + current

							if (!tree.edgeExists(current, parent(current))) {
								tree = tree.addEdge(current, parent(current), getEdgeWeight(current, parent(current))
									.get)
							}

							if (stillConnected) {
								for (other <- this.getAdjacent(current)) {
									if (!visited.contains(other)) {
										val newDist: Int = this.getEdgeWeight(current, other).get
										if (!dist.contains(other) || newDist < dist(other)) {
											dist = dist + (other -> newDist)
											parent = parent + (other -> current)
										}
									}
								}
							}
						}
					}
				}
				if (tree != null && tree.getVertices.equals(this.getVertices) && stillConnected) {
					Some(tree)
				}
				else {
					None
				}
			}

			/*
			procedure 2OPT(Graph graph, Sequence tour)
			best = tour
			bestDist = pathLength(tour)
			while there is still improvement do
				for i = 0 : : : length(best) 􀀀 1 do
					for j = i + 1 : : : length(best) do
						newTour = 2OPT-Swap(best; i; j)
						dist = pathLength(newTour)
							if dist < bestDist then
								best = newTour
								bestDist = dist
							end if
					end for
				end for
			end while
			return best
			end procedure

				Note: If you start/end at a particular node (depot), then you must remove this from the search as an eligible
			candidate for swapping, as reversing the order will cause an invalid path.
				*/

			/**
			 * Finds an approximate answer to the Traveling Salesperson Problem using the greedy algorithm
			 * 2-Opt.
			 * @return an approximate answer to the Traveling Salesperson Problem
			 */
			def greedyTSP():Seq[Edge[T]] = {
				var vertices:Seq[T] = getVertices.toSeq
				vertices = vertices :+ vertices.head
				greedyTSP(vertices)
			}


			/**
			 * Finds an approximate answer to the Traveling Salesperson Problem using the greedy algorithm
			 * 2-Opt.
			 * @param initialTour the initial tour
			 * @return an approximate answer to the Traveling Salesperson Problem
			 */
			def greedyTSP(initialTour:Seq[T]):Seq[Edge[T]] = {
				if (initialTour.nonEmpty) {
					var best = initialTour
					var bestDist = pathLength(initialTour).get
					var foundImprovement: Boolean = true

					while (foundImprovement) {
						foundImprovement = false
						for (i <- Range(1, best.length - 1)) {
							for (j <- Range(i + 1, best.length - 1)) {
								var newTour = twoOptSwap(best, i, j)
								var dist = pathLength(newTour).get
								if (dist < bestDist) {
									best = newTour
									bestDist = dist
									foundImprovement = true
								}
							}
						}
					}
					makeEdgeTour(best)
				}
				else {
					Seq[Edge[T]]()
				}
			}


      /**
       * Makes a tour of edges from the given tour of vertices
       * @param vertexTour
       * @return
       */
      def makeEdgeTour(vertexTour:Seq[T]):Seq[Edge[T]] = {
        var bestTour: Seq[Edge[T]] = Seq()
        for (vertices <- vertexTour.sliding(2)) {
        val newEdge = getEdge(vertices(0), vertices(1)).get
        bestTour = bestTour :+ newEdge
      }
        bestTour
      }


			/**
			 * Performs a 2-opt swap on the given tour.
			 * @param tour the tour
			 * @param i the starting index for the swap
			 * @param k the ending index for the swap
			 * @return the new sequence
			 */
			def twoOptSwap(tour:Seq[T], i:Int, k:Int): Seq[T] = {
				var prefix:Seq[T] = tour.slice(0, i)
				var mid:Seq[T] = tour.slice(i, k)
				var end:Seq[T] = tour.slice(k, tour.length)
				prefix ++ mid.reverse ++ end
			}

			def branchBoundTSP:Seq[Edge[T]] = {
        // TODO call other branchBoundTSP and give it a default heuristic fn
        branchBoundTSP(defaultHeur)
			}


			def branchBoundTSP(heur:(Graph[T], Seq[T]) => Long):Seq[Edge[T]] = {
        val depot:T = getVertices.head;
        var stack:util.Stack[Seq[T]] = new util.Stack[Seq[T]]()
        var best:Seq[T] = null;
        var isFirstSolution:Boolean = true
        stack.push(Seq(depot))
        var minCost:Long = 0;

        while (!stack.isEmpty) {
          val current: Seq[T] = stack.pop()
          if (isFirstSolution || (current.size == 1) || (pathLength(current).get + heur(this,
            current) < minCost)) { // minCost is never getting updated
            isFirstSolution = false
            if (current.length == this.getVertices.size + 1) {
              best = current
              minCost = pathLength(current).get
            }
            else {
              if (current.size < getVertices.size) {
                for (vertex <- this.getVertices) {
                  if (!current.contains(vertex)) {
                    stack.push(current :+ vertex)
                  }
                }
              }
              else {
                stack.push(current :+ depot)
              }
            }
          }
        }

				makeEdgeTour(best)
			}


      def defaultHeur(graph:Graph[T], tour:Seq[T]):Long = {
        // return cheapest edge weight * number of edges needed to finish the tour
        // for connected graph, edges = vertices - 1
        // need to go back to depot as well, so + 1
        val numNeeded:Long = graph.getVertices.size - tour.size

        val cheapestWeight:Long =  {
          var minWeight:Long = graph.getEdgeWeight(tour.head, tour(1)).get
          for (vertices <- tour.sliding(2)) {
            val currentWeight:Long = graph.getEdgeWeight(vertices(0), vertices(1)).get
            if (currentWeight < minWeight) {
              minWeight = currentWeight
            }
          }
          minWeight
        }

        cheapestWeight * numNeeded
      }


			/*
      Loads a graph from a TSP file
      */
			def fromTSPFile(fileName:String):Graph[Int] =
			{
				//create an empty graph
				val emptyGraph = Graph[Int](false)

				//load the XML file
				val tspXML = loadFile(fileName)

				//get all the veritices
				val vertices = tspXML \\ "vertex"

				//add in all the vertices
				val graph = Range(0, vertices.size).foldLeft(emptyGraph)((g,v) => g.addVertex(v))

				//add in all the edges - they are part of each xml vertex
				vertices.zipWithIndex.foldLeft(graph)((g,t) => addXMLEdges(g, t._1, t._2))
			}

			/*
      Add in edges assume the vertices exist
      */
			private def addXMLEdges(graph:Graph[Int], xmlEdges:Node, start:Int):Graph[Int] =
			{
				//parse all the edges - tuples of (destination, weight)
				val edges = (xmlEdges \ "edge").map(e => (e.text.toInt, e.attributes("cost").text.toDouble.toInt))

				//remove the edges that already exist
				val newEdges = edges.filterNot(e => graph.edgeExists(start, e._1))

				//add in new edges
				newEdges.foldLeft(graph)((g,e) => g.addEdge(start, e._1, e._2))
			}


      /**
       * Returns a human-readable representation of this graph.
       *
       * @return a human-readable representation of this graph
       */
      override def toString: String = _adjacencyMap.keys.mkString(";")
		}
	}


	/**
	 * A container for an edge on a graph.
	 *
	 * @param source the label of the source of the edge
	 * @param destination the label of the destination of the edge
	 * @param weight the weight of the edge
	 * @tparam T the type stored in the vertices
	 */
	class Edge[T](val source:T, val destination:T, val weight:Int) extends Ordered[Edge[T]] {
		/**
		 * Compares this object with another object.
		 * @param other the other object to compare this object to
		 * @return -1 if this edge's weight < other edge's weight
		 *         0 if this edge's weight == other edge's weight
		 *         1 if this edge's weight > other edge's weight
		 */
		def compare(other:Edge[T]):Int = {
			var result:Int = 0;
			if (this.weight < other.weight) {
				result = -1;
			}
			else if (this.weight == other.weight) {
				result = 0;
			}
			else {
				result = 1;
			}
			result;
		}


		override def equals(other:Any): Boolean = {
			other match {
				case edge:Edge[T] => {
					if (this.source.equals(edge.source) && this.destination.equals(edge.destination)) {
						true
					}
					else {
						false
					}
				}
				case _ => false
			}
		}


		override def toString(): String = {
			val outputString = source + "," + destination + "," + weight
			outputString
		}
	}


	def main(args:Array[String])
	{
		var example_graph:Graph[String] = Graph.fromCSVFile(false, "graph_10_319.csv")
		//println(example_graph)
		println(example_graph.greedyTSP())
    println(example_graph.branchBoundTSP)
	}
}