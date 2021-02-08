import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

object Solution {
  val limit = 10000000

  def solution(a: Array[Int]) = { // 100% O(n*log(n))
    val lastIndex = a.length * 2 - 1
    borders(a).zipWithIndex.scanLeft((0, 0L, false)) {
      case ((openDiscs, count, _), (borderStart, i)) =>
        if (borderStart) {
          val newCount = count + openDiscs
          val overLimit = newCount > limit
          (openDiscs + 1, if (overLimit) -1 else newCount, overLimit || i == lastIndex)
        } else { // border == end
          (openDiscs - 1, count, i == lastIndex)
        }
    }
  }.find(_._3).map(_._2.toInt).getOrElse(0)

  private def borders(a: Array[Int]) = {
    val buffer = new ArrayBuffer[(Long, Boolean)](a.length * 2)
    for (i <- a.indices) {
      buffer.append((i.toLong - a(i), true))
      buffer.append((i.toLong + a(i), false))
    }

    val rangePoints = buffer.toArray
    Sorting.quickSort(rangePoints)((x: (Long, Boolean), y: (Long, Boolean)) => {
      val ((a, borderA), (b, borderB)) = (x, y)
      if (a != b) a compareTo b
      else if (borderA) -1
      else if (borderB) 1
      else 0
    })
    rangePoints.iterator.map(_._2)
  }
}

import scala.util.Random

val random = new Random()

val f = Solution.solution _

val example = Array(1, 5, 2, 1, 4, 0)

f(example) // 11
