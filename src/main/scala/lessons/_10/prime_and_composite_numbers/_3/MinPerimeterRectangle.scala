package lessons._10.prime_and_composite_numbers._3

object Solution {
  import Iterator.from

  def solution(n: Int): Int = { // T: O(sqrt n) | S: O(1)
    val sqrt   = math.sqrt(n.toDouble).toInt
    val (a, b) =
      if (sqrt * sqrt == n) (sqrt, sqrt)
      else from(sqrt, -1).find(n % _ == 0).map(i => (i, n / i)).get
    2 * (a + b)
  }
}
