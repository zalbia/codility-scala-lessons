object Solution {

  /**
   * Assumptions:
   * - N is an integer within the range [0..100,000];
   * - the elements of A are all distinct;
   */
  def solution(arr: Array[Int]): Int = {
    def findMissing(n: Int, as: List[Int]): Int = {
      if (as.isEmpty || as.head != n + 1) n + 1
      else findMissing(n + 1, as.tail)
    }
    if (arr.length == 0) 1 // 0
    else findMissing(0, arr.toList.sorted)
  }
}
