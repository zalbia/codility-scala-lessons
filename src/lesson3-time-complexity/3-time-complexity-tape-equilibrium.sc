object Solution {

  /**
   * - N is an integer within the range [2..100,000];
   * - each element of array A is an integer within the range [-1,000..1,000].
   * - array has [2..100,000] elements
   */
  def solution(a: Array[Int]): Int = { // 100%
    val sums = a.scan(0)(_ + _).drop(1).dropRight(1)
    val reverseSums = a.scanRight(0)(_ + _).dropRight(1).drop(1)
    sums.zip(reverseSums).map {
      case (a, b) => math.abs(a - b)
    }.min
  }

}

Solution.solution(Array(3,1,2,4,3))