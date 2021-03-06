package lessons._05.prefix_sums._4
import utest._

import scala.util.Random

object PassingCarsTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example")(check(Array(0, 1, 0, 1, 1), 5))
    test(check(Array(0), 0))
    test(check(Array(0, 0), 0))
    test(check(Array(0, 1), 1))
    test(check(Array(1, 0), 0))
    test(check(Array(0, 0, 0), 0))
    test(check(Array(0, 0, 1), 2))
    test(check(Array(0, 1, 0), 1))
    test(check(Array(0, 1, 1), 2))
    test(check(Array(1, 0, 0), 0))
    test(check(Array(0, 1, 0, 1), 3))
    test("all eastward")(check(Array.fill(100000)(0), 0))
    test("all westward")(check(Array.fill(100000)(1), 0))
    test("99999 cars eastward, 1 westward")(check(Array.fill(99999)(0) ++ Array(1), 99999))
    test("extreme-alternating")(check(Array.iterate(1, 100000)(n => if (n == 0) 1 else 0), -1))
    test("stress-test")(f(Array.fill(100000)(random.nextInt(2))))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
