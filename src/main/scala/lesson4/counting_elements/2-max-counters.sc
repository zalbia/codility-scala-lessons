object Solution {
  /**
   * Assumptions:
   * - N and M are integers within the range [1..100,000];
   * - M = a.size
   * - each element of array A is an integer within the range [1..N + 1]
   */
  def solution(n: Int, as: Array[Int]): Array[Int] = { // 88%
    val counterArray = Array.ofDim[Int](n)
    val zeros = Array.ofDim[Int](n)
    var maxCounter = 0
    var offset = 0
    as.foreach { a =>
      if (a <= n) {
        counterArray(a - 1) += 1
        if (counterArray(a - 1) + offset > maxCounter)
          maxCounter = counterArray(a - 1) + offset
      } else if (a == n + 1) {
        offset = maxCounter
        Array.copy(zeros, 0, counterArray, 0, zeros.length)
      }
    }
    counterArray.map(_ + offset)
  }
}

Solution.solution(5, Array(3,4,4,6,1,4,4))