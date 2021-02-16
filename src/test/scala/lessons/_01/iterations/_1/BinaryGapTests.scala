package lessons._01.iterations._1
import utest._

object BinaryGapTests extends TestSuite {
  val solutions = Array(
    Solution.solution _,
    Alternative1.solution _,
    Alternative2.solution _,
    Alternative3.solution _
  )

  val tests = utest.Tests {
    test(check(1, 0))
    test(check(2, 0))
    test(check(4, 0))
    test(check(9, 2))
    test(check(10, 1))
    test(check(32, 0))
    test(check(37, 2))
    test(check(273, 3))
    test(check(4375, 3))
    test(check(4096, 0))
    test(check(Int.MaxValue, 0))
    test(check(1073741825, 29))
  }

  private def check(n: Int, expected: Int): Unit =
    solutions.foreach { f =>
      println(s"binary($n): ${n.toBinaryString}")
      val actual = f(n)
      assert(actual == expected)
    }
}
