import org.scalatest.flatspec.AnyFlatSpec
import graph.Graph

/**
Performs a series of tests on implementations of the graph
 */
class UndirectedGraphTest extends AnyFlatSpec
{

	/**
	Check empty graphs
	*/
	"An empty graph"  should "be empty" in
	{
		val graph = Graph[Int](false)

		assert(graph.getVertices.isEmpty)
	}

	it should "not have edges" in
	{
		val graph = Graph[Int](false)

		assert(!graph.edgeExists(1,2))
		assert(!graph.edgeExists(1,1))
	}

	it should "not have a path" in
	{
		val graph = Graph[String](false)
		val path = Seq("foo", "bar")

		assert(graph.pathLength(path).isEmpty)
	}

	it should "not have a shortest path" in
	{
		val graph = Graph[String](false)

		assert(graph.shortestPathBetween("foo", "bar").isEmpty)
	}

	it should "not have a MST" in
	{
		val graph = Graph[String](false)

		assert(graph.minimumSpanningTree.isEmpty)
	}

	it should "have an empty tour" in
	{
		val graph = Graph[String](false)
		
		assert(graph.greedyTSP.isEmpty)
	}

	it should "have an empty tour (DYN)" in
	{
		val graph = Graph[String](false)
		
		assert(graph.dynamicTSP.isEmpty)
	}

	it should "have an empty tour (Genetic)" in
	{
		val graph = Graph[String](false)
		
		assert(graph.geneticTSP.isEmpty)
	}

	it should "have an empty tour (B&B)" in
	{
		val graph = Graph[String](false)
		
		assert(graph.branchBoundTSP.isEmpty)
	}

	it should "have an empty tour (B&B w/ heuristic)" in
	{
		val graph = Graph[String](false)
		
		assert(graph.branchBoundTSP((g, s) => 0).isEmpty)
	}
}
