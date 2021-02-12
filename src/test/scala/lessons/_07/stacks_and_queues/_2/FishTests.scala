package lessons._07.stacks_and_queues._2
import utest._

import scala.util.Random

object FishTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val extreme = 10000 to 1000000000 by 10000
  val reverseExtreme = extreme.reverse

  val tests = Tests {
    test { check(Array(4, 3, 2, 1, 5), Array(0, 1, 0, 0, 0), 2) }
    test { check(Array(1), Array(1), 1) }
    test { check(Array(1), Array(0), 1) }
    test { check(Array(1), Array(1), 1) }
    test { check(Array(1, 2), Array(0, 0), 2) }
    test { check(Array(1, 2), Array(0, 0), 2) }
    test { check(Array(1, 2), Array(1, 0), 1) }
    test { check(Array(1, 2), Array(0, 1), 2) }
    test { check(Array(1, 2), Array(1, 1), 2) }
    test { check(Array(2, 1), Array(0, 0), 2) }
    test { check(Array(2, 1), Array(0, 0), 2) }
    test { check(Array(2, 1), Array(1, 0), 1) }
    test { check(Array(2, 1), Array(0, 1), 2) }
    test { check(Array(2, 1), Array(1, 1), 2) }
    test { check(Array(4, 3, 2, 1, 5), Array(0, 1, 0, 1, 0), 2) }
    test { check(extreme.toArray, Array(1) ++ Array.fill(99999)(0), 99999) }
    test { check(reverseExtreme.toArray, Array(1) ++ Array.fill(99999)(0), 1) }
    test { f(Random.shuffle[Int, IndexedSeq](extreme).toArray, Array.fill(100000)(random.nextInt(2))) }
  }

  def check(a: Array[Int], b: Array[Int], expected: Int): Unit = {
    val result = f(a, b)
    assert(result == expected)
  }
}
