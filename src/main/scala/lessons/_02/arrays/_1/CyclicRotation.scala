package lessons._02.arrays._1

// N and K are integers within the range [0..100];
// each element of array A is an integer within the range [−1,000..1,000].
object Solution {
  // declarative solution
  // allocates 3 iterators, then an array
  // spot of unsafety: % (division by 0)
  def solution(a: Array[Int], k: Int): Array[Int] = { // 100% | time: O(n) | space: O(n)
    val complementPivot = if (a.isEmpty) 0 else a.length - k % a.length
    (a.iterator.drop(complementPivot) ++ a.iterator.take(complementPivot)).toArray
  }
}

object Alternative1 {
  // concise, but slow & expensive
  // allocates arrays k % n times
  @scala.annotation.tailrec
  def solution(a: Array[Int], k: Int): Array[Int] = // 100% | time: O(n * k % n) | space: O(n)
    if (a.isEmpty || k % a.length == 0) a
    else solution(a.last +: a.dropRight(1), k % a.length - 1)
}

object Alternative2 {
  // fast, but verbose & mechanical af
  // array access is unsafe
  def solution(a: Array[Int], k: Int): Array[Int] = { // 100% | time: O(n) | space: O(n)
    if (a.isEmpty || k % a.length == 0) a
    else {
      val rotation = Array.ofDim[Int](a.length) // only one array allocation
      for (i <- a.indices) { // allocates a Range
        val j = i - k % a.length
        rotation(i) = a(if (j < 0) a.length + j else j)
      }
      rotation
    }
  }
}

import utest._

import scala.util.Random

object CyclicRotationTests extends TestSuite {
  val solutions = Array(Solution.solution _, Alternative1.solution _, Alternative2.solution _)
  val random = new Random()

  val tests = Tests {
    test("example 1") { check(Array(3, 8, 9, 7, 6), 3, Array(9, 7, 6, 3, 8)) }
    test("example 2") { check(Array(1, 2, 3, 4), 4, Array(1, 2, 3, 4)) }
    test("example 3") { check(Array(0, 0, 0), 4, Array(0, 0, 0)) }
    test("failing case") { check(Array(1, 1, 2, 3, 5), 42, Array(3, 5, 1, 1, 2)) }
    test { check(Array(), 4, Array()) }
    test { check(Array(1), 4, Array(1)) }
    test { check(Array(1, 2, 3, 4), 3, Array(2, 3, 4, 1)) }
    val (min, max, maxN) = (-1000, 1000, 100)
    val step = max * 2 / maxN
    test("extreme") {
      check(
        a = (min to max by step).toArray,
        k = 100,
        expected = ((-980 to max by step).toIterator ++ Iterator.single(-1000)).toArray
      )
    }
  }

  private def check(a: Array[Int], k: Int, expected: Array[Int]): Unit =
    solutions.foreach { f =>
      val actual = f(a, k).toList
      val expectedList = expected.toList
      assert(actual == expectedList)
    }
}
