object Solution {
  // A and B are integers within the range [0..2,000,000,000];
  // K is an integer within the range [1..2,000,000,000];
  // A ≤ B.
  def solution(a: Int, b: Int, k: Int): Int = { // 100%
    val divA = if ((a % k) == 0) 1 else 0
    (b / k) - (a / k) + divA // b_div - a_div, add div(a)
  }
}

val soln = Solution.solution _

soln(0, 1, 1) // 2
soln(0, 0, 1) // 1
soln(6, 11, 2) // 3
soln(1, 10, 5) // 2
soln(10, 10, 17) // 0
soln(10, 10, 5) // 1
soln(11, 345, 17) // 20
soln(0, 2000000000, 10) // 200000001
soln(0, 2000000000, 1) // 2000000001
