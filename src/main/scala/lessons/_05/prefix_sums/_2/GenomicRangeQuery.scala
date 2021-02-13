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
      val rangeEnd   = q(k)
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
