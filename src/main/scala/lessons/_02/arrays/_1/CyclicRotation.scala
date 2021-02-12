package lessons._02.arrays._1

// N and K are integers within the range [0..100];
// each element of array A is an integer within the range [−1,000..1,000].
object Solution {
  // declarative solution
  // allocates 3 iterators, then an array
  // spot of unsafety: % (division by 0)
  def solution(a: Array[Int], k: Int): Array[Int] = { // 100% | time: O(n) | space: O(n)
    val complementPivot = if (a.isEmpty) 0 else a.length - k % a.length
    (a.iterator.drop(complementPivot) ++ a.iterator.take(complementPivot)).toArray
  }
}

object Alternative1 {
  // concise, but slow & expensive
  // allocates arrays k % n times
  @scala.annotation.tailrec
  def solution(a: Array[Int], k: Int): Array[Int] = // 100% | time: O(n * k % n) | space: O(n)
    if (a.isEmpty || k % a.length == 0) a
    else solution(a.last +: a.dropRight(1), k % a.length - 1)
}

object Alternative2 {
  // fast, but verbose & mechanical af
  // minimal allocations
  // array access is unsafe
  def solution(a: Array[Int], k: Int): Array[Int] = { // 100% | time: O(n) | space: O(n)
    if (a.isEmpty || k % a.length == 0) a
    else {
      val rotation = Array.ofDim[Int](a.length) // only one array allocation
      for (i <- a.indices) { // allocates a Range
        val complement = i - k % a.length
        val offset = if (complement < 0) a.length + complement else complement
        rotation(i) = a(offset)
      }
      rotation
    }
  }
}
