package lessons._04.counting_elements._2

// N and M are integers within the range [1..100,000];
// M = a.size
// each element of array A is an integer within the range [1..N + 1]

object Solution {
  // mostly procedural solution
  def solution(n: Int, a: Array[Int]): Array[Int] = { // 100% | O(n + m)
    val counters = Array.ofDim[Int](n)
    var maxCounter = 0
    var offset = 0
    for (elem <- a) {
      if (elem <= n) {
        val counterIdx = elem - 1
        if (counters(counterIdx) < offset)
          counters(counterIdx) = offset + 1
        else
          counters(counterIdx) += 1

        if (counters(counterIdx) > maxCounter)
          maxCounter = counters(counterIdx)
      } else if (elem == n + 1)
        offset = maxCounter
    }
    counters.map(math.max(offset, _))
  }
}

object OopAlternative {
  // "OOP" solution
  def solution(n: Int, a: Array[Int]): Array[Int] = {
    val maxCounters = MaxCounters.create(n)
    a.foreach(elem =>
      if (elem <= n) maxCounters.maxCount(elem)
      else maxCounters.max()
    )
    maxCounters.finalCounter()
  }

  // could declare inside solution, just wouldn't read as well
  private[_2] class MaxCounters private(
    private val counters: Array[Int],
    private var maxCounter: Int = 0,
    private var offset: Int = 0
  ) {
    def maxCount(elem: Int): Unit = {
      if (count(elem) < offset)
        setCount(elem)(offset + 1)
      else
        setCount(elem)(count(elem) + 1)

      if (count(elem) > maxCounter)
        maxCounter = count(elem)
    }

    private def count(elem: Int): Int =
      counters(elem - 1)
    private def setCount(elem: Int)(value: Int): Unit =
      counters(elem - 1) = value

    def max(): Unit = offset = maxCounter

    def finalCounter(): Array[Int] = counters.map(math.max(offset, _))
  }

  private[_2] object MaxCounters {
    def create(n: Int) = new MaxCounters(Array.ofDim(n))
  }
}

import utest._

import scala.util.Random

object MaxCountersTests extends TestSuite {
  val random = new Random()
  val solutions = Array(
    Solution.solution _,
    OopAlternative.solution _
  )

  val tests = Tests {
    test("example") {
      check(n = 5, a = Array(3, 4, 4, 6, 1, 4, 4), expected = Array(3, 2, 2, 4, 2))
    }
    test("extreme-ones") {
      check(n = 100000, a = Array.fill(100000)(1), expected = (100000 :: List.fill(99999)(0)).toArray)
    }
    test("extreme-max-counters") {
      check(n = 100000, a = Array.fill(100000)(100001), expected = Array.fill(100000)(0))
    }
  }

  private def check(n: Int, a: Array[Int], expected: Array[Int]): Unit = {
    solutions.foreach { f =>
      val maxCounters = f(n, a).toList
      val length = maxCounters.length
      assert(length == n)
      assert(maxCounters == expected.toList)
    }
  }
}
