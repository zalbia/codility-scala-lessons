package leetcode.find_lucky

object Solution {
  def findLucky(arr: Array[Int]): Int = {
    val lucky = arr
      .foldLeft(Map.empty[Int, Int]) { (map, n) =>
        map.get(n).fold(map + (n -> 1))(count => map.updated(n, count + 1))
      }
      .collect { case (n, count) if n == count => n }

    if (lucky.nonEmpty) lucky.max else -1
  }
}
