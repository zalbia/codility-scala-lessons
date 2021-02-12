package lessons._03.time_complexity._2
import utest._
import scala.util.Random

object PermMissingElemTests extends TestSuite {
  val random = new Random()
  val solutions = Array(Solution.solution _/*, Alternative.solution _*/)

  val tests = Tests {
    test("example") { check(Array(2,3,1,5), 4) }
    test { check(Array(), 1) }
    test { check(Array(2, 3), 1) }
    test { check(Array(1, 3), 2) }
    test { check(Array(3, 1), 2) }
    test { check(Array(1, 2), 3) }
    test { check(Array(2, 1), 3) }
    test("extreme") { check((2 to 100000).toArray, 1) }
    test("extreme-shuffle") { check(random.shuffle[Int, IndexedSeq](2 to 100000).toArray, 1) }
  }

  private def check(a: Array[Int], expected: Int): Unit =
    solutions.foreach { f =>
      val actual = f(a)
      assert(actual == expected)
    }
}
