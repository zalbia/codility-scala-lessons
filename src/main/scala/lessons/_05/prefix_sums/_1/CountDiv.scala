package lessons._05.prefix_sums._1

object Solution {
  // A and B are integers within the range [0..2,000,000,000];
  // K is an integer within the range [1..2,000,000,000];
  // A ≤ B.
  def solution(a: Int, b: Int, k: Int): Int = // 100%
    (b / k) - (a / k) + (if ((a % k) == 0) 1 else 0)
}
