package lessons._04.counting_elements._3

// N is an integer within the range [1..100,000];
// each element of array A is an integer within the range [−1,000,000..1,000,000].
object Solution {
  // counts each number from 0 to n
  // then finds the smallest missing positive integer
  def solution(a: Array[Int]): Int = { // 100% | time: O(n) | space: O(n)
    val n = a.length
    val seen = Array.fill(n)(false)
    for (elem <- a if 0 < elem && elem <= n)
      seen(elem - 1) = true
    for (i <- seen.indices if !seen(i))
      return i + 1
    n + 1
  }
}

object Slow {
  // concise, but sorting makes it logarithmically slower
  def solution(a: Array[Int]): Int = // 100% | time: O(n * log(n)) | space: O(n)
    a.sorted.iterator.filter(_ > 0).takeWhile(_ <= 100000).fold(1)((smallest, a) =>
      if (smallest == a) a + 1 else smallest
    )
}