object Solution {
  /**
   * Assumptions:
   * - N is an integer within the range [1..100,000];
   * - each element of array A is an integer within the range [1..1,000,000,000].
   */
  def solution(A: Array[Int]): Int = { // 100%
    val N = A.length
    N match {
      case 1 => if (A(0) == 1) 1 else 0
      case _ =>
        val sorted = A.sorted
        val sequenced = sorted.iterator.sliding(2)
          .forall(pair => pair(1) - pair(0) == 1)
        val is1ToN = sorted(0) == 1 && sorted(N - 1) == N
        if (sequenced && is1ToN) 1 else 0
    }
  }
}

val soln = Solution.solution _

soln(Array(2)) // 0
soln(Array(4, 1, 3, 2)) // 1
soln(Array(4, 3, 2)) // 0
soln(Array(4, 1, 3)) // 0
soln(Array(1, 4, 1)) // 0
soln(Array(1, 1000000000)) // 0
soln((1 to 100000).toArray) // 1
soln(Array.fill(100000)(1000000000)) // 0