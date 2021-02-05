object Solution {
  //  N is an integer within the range [1..100,000];
  //  M is an integer within the range [1..50,000];
  //  each element of arrays P, Q is an integer within the range [0..N − 1];
  //  P[K] ≤ Q[K], where 0 ≤ K < M;
  //  string S consists only of upper-case English letters A, C, G, T.
  def solution(s: String, p: Array[Int], q: Array[Int]): Array[Int] = // 62%
    (p zip q) map ((p: Int, q: Int) => impactFactor(s).slice(p, q + 1).min).tupled

  def impactFactor(s: String) = {
    s.map {
      case 'A' => 1
      case 'C' => 2
      case 'G' => 3
      case 'T' => 4
    }.toArray
  }
}

val soln = Solution.solution _

soln("CAGCCTA", Array(2,5,0), Array(4,5,6)) // 2,4,1