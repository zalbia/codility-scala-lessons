package lessons._03.time_complexity._1

// X, Y and D are integers within the range [1..1,000,000,000];
// X ≤ Y.
object Solution {
  // more math problem than programming...
  def solution(x: Int, y: Int, d: Int): Int = { // 100% | T: O(1) | S: O(1)
    val delta = y - x
    delta / d + (if (delta % d == 0) 0 else 1)
  }
}
