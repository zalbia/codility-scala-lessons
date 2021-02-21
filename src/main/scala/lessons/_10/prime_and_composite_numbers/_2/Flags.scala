package lessons._10.prime_and_composite_numbers._2

object Solution {
  import Iterator.from
  import scala.collection.mutable.ArrayBuffer

  def solution(a: Array[Int]): Int = { // 100% | T: O(sqrt(n^2)) -> O(n) | S: O(n)
    if (a.length < 3)
      return 0
    val peaks = findPeaks(a)
    if (peaks.length < 2)
      return peaks.length
    from(2).find { k => // O(sqrt n)
      var p     = peaks.head
      var flags = 1
      for (q <- peaks if flags <= k) // O(n)
        if (q - p >= k) {
          p = q
          flags += 1
        }
      flags < k
    }.map(_ - 1).getOrElse(0)
  }

  private def findPeaks(a: Array[Int]): Array[Int] = {
    val peaks = new ArrayBuffer[Int](a.length - 2)
    for (i <- 1 until a.length - 1)
      if (a(i) > a(i - 1) && a(i) > a(i + 1))
        peaks += i
    peaks.toArray
  }
}
