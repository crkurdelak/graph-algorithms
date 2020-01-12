import org.scalatest.FlatSpec
import graph.Graph

/**
Performs a series of tests on implementations of the graph
*/
class DirectedGraphTest extends FlatSpec
{
	/**
	Check empty graphs
	*/
	"An empty graph" should "be empty" in
	{
		val graph = Graph[Int](true)

		assert(graph.getVertices.isEmpty)
	}

	it should "not have edges" in
	{
		val graph = Graph[Int](true)

		assert(!graph.edgeExists(1,2))
		assert(!graph.edgeExists(1,1))
	}

}