package lessons._04.counting_elements._1
import utest._

import scala.util.Random

object FrogRiverOneTests extends TestSuite {
  val random = new Random()
  val solutions = Array(Solution.solution _, Alternative.solution _)

  val tests = Tests {
    test("example1") { check(5, Array(1, 3, 1, 4, 2, 3, 5, 4), 6) }
    test { check(2, Array(1), -1) }
    test { check(2, Array(1), -1) }
    test("extreme") { check(100000, (1 to 100000).toArray, 99999) }
    test("random extreme") { check(100000, random.shuffle[Int, IndexedSeq](1 to 100000).toArray, 99999) }
    test("1 short of max") { check(100000, (1 to 99999).toArray, -1) }
  }

  def check(n: Int, a: Array[Int], expected: Int): Unit = {
    solutions.foreach { f =>
      val result = f(n, a)
      assert(result == expected)
    }
  }

}


