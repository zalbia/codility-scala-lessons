package lessons._10.prime_and_composite_numbers._1

import scala.collection.Iterator.iterate

object Solution {
  def solution(n: Int): Int = {
    val (count, finalSquare) = iterate(1L)(_ + 1)
      .takeWhile(i => i * i <= n)
      .foldLeft((0, 1L)) { case ((count, _), i) =>
        (if (n % i == 0 && i * i != n) count + 2 else count, i * i)
      }
    if (finalSquare == n) count + 1 else count
  }
}
