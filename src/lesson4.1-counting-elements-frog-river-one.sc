import scala.collection.mutable

object Solution {
  def solution(x: Int, as: Array[Int]): Int = { // 100%
    val spots = mutable.Set[Int]()
    var earliestTime = -1
    as.zipWithIndex.takeWhile { case (a, k) =>
      spots += a
      if (spots.size == x) earliestTime = k
      spots.size != x
    }
    earliestTime
  }
}

assert(Solution.solution(5, Array(1, 3, 1, 4, 2, 3, 5, 4)) == 6)
assert(Solution.solution(2, Array(1)) == -1)
