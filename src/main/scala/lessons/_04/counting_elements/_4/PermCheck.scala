package lessons._04.counting_elements._4

object Solution {

  /**
   * Assumptions:
   * - N is an integer within the range [1..100,000];
   * - each element of array A is an integer within the range [1..1,000,000,000].
   */
  def solution(a: Array[Int]): Int = { // 100%
    val n = a.length
    n match {
      case 1 => if (a(0) == 1) 1 else 0
      case _ =>
        val sorted    = a.sorted
        val sequenced = sorted.iterator
          .sliding(2)
          .forall(pair => pair(1) - pair(0) == 1)
        val is1ToN    = sorted(0) == 1 && sorted(n - 1) == n
        if (sequenced && is1ToN) 1 else 0
    }
  }
}
