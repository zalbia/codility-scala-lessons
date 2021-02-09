package lessons._05.prefix_sums._2

object Solution {
  def impactOf(base: Char) = base match {
    case 'A' => 1
    case 'C' => 2
    case 'G' => 3
    case 'T' => 4
  }

  val numberOfBases = 4

  //  N is an integer within the range [1..100,000];
  //  M is an integer within the range [1..50,000];
  //  each element of arrays P, Q is an integer within the range [0..N − 1];
  //  P[k] ≤ Q[k], where 0 ≤ k < M;
  //  string S consists only of upper-case letters A, C, G, T.
  def solution(s: String, p: Array[Int], q: Array[Int]): Array[Int] = // 100% | O(m + n)
    runQueries(p, q, prefixMinima(s, s.length))

  private def runQueries(p: Array[Int], q: Array[Int], prefixMinima: Array[Array[Int]]) = {
    val arr = Array.ofDim[Int](p.length)
    for (k <- p.indices) {
      val rangeStart = p(k)
      val rangeEnd = q(k)
      for (impactLessOne <- prefixMinima(rangeEnd).indices if arr(k) == 0) {
        val impact = impactLessOne + 1
        val prefix = prefixMinima(rangeEnd)(impactLessOne)
        if (prefix >= rangeStart) arr(k) = impact
      }
    }
    arr
  }

  private def prefixMinima(s: String, n: Int) = {
    val prefixMinima = Array.fill(n, 4)(-1)
    for (row <- s.indices) {
      if (row > 0)
        for (column <- prefixMinima(row).indices)
          prefixMinima(row)(column) = prefixMinima(row - 1)(column)
      val impact = impactOf(s.charAt(row))
      prefixMinima(row)(impact - 1) = row
    }
    prefixMinima
  }
}

import utest._

import scala.util.Random

object GenomicRangeQueryTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example") { check("CAGCCTA", Array(2, 5, 0), Array(4, 5, 6), Array(2,4,1)) }
    test { check("A", Array(0), Array(0), Array(1)) }
    test { check("C", Array(0), Array(0), Array(2)) }
    test { check("G", Array(0), Array(0), Array(3)) }
    test { check("T", Array(0), Array(0), Array(4)) }
    test { check("ACG", Array(0, 2), Array(2, 2), Array(1, 3)) }
    test("extreme") { check("A" * 100000, Array.fill(50000)(0), (0 until 50000).toArray, Array.fill(50000)(1)) }
  }

  def check(s: String, p: Array[Int], q: Array[Int], expectedArr: Array[Int]) = {
    val result = f(s, p, q).toList
    val expected = expectedArr.toList
    assert(result == expected)
  }
}

