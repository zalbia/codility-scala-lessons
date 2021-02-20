package lessons._10.prime_and_composite_numbers._2

object Solution {
  import Iterator.from
  import scala.collection.mutable.ArrayBuffer

  def solution(a: Array[Int]): Int = // 100% | T: O(sqrt(n^2)) -> O(n) | S: O(n)
    if (a.length < 3) 0
    else {
      val peaks = mkPeaks(a)
      if (peaks.length < 2)
        peaks.length
      else {
        from(2).find { k =>
          var count = 1
          var p     = peaks.head
          for (q <- peaks if count <= k)
            if (q - p >= k) {
              p = q
              count += 1
            }
          count < k
        }.map(_ - 1).getOrElse(0)
      }
    }

  private def mkPeaks(a: Array[Int]): Array[Int] = {
    val peaks = new ArrayBuffer[Int](a.length - 2)
    for (i <- 1 until a.length - 1) {
      val x = a(i - 1)
      val y = a(i)
      val z = a(i + 1)
      if (y > x && y > z)
        peaks += i
    }
    peaks.toArray
  }
}
