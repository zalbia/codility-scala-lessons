package lessons._06.sorting._3

import scala.collection.mutable.ArrayBuffer
import scala.util.Sorting

// N is an integer within the range [0..100,000];
// each element of array A is an integer within the range [0..2,147,483,647].
object Solution {
  val limit = 10000000

  // functional early exits are verbose, but explicit
  def solution(a: Array[Int]): Int = { // 100% O(n*log(n))
    val lastIndex = a.length * 2 - 1
    borders(a).zipWithIndex.scanLeft((0, 0L, false)) {
      case ((openDiscs, count, _), (borderStart, i)) =>
        if (borderStart) {
          val newCount = count + openDiscs
          val overLimit = newCount > limit
          (openDiscs + 1, if (overLimit) -1 else newCount, overLimit || i == lastIndex)
        } else { // borderEnd
          (openDiscs - 1, count, i == lastIndex)
        }
    }
  }.find(_._3).map(_._2.toInt).getOrElse(0)

  // sort range-points then return border order
  private def borders(a: Array[Int]) = {
    val buffer = new ArrayBuffer[(Long, Boolean)](a.length * 2)
    for (i <- a.indices) {
      buffer.append((i.toLong - a(i), true))
      buffer.append((i.toLong + a(i), false))
    }

    val rangePoints = buffer.toArray // we sort this in place
    Sorting.quickSort(rangePoints)((x: (Long, Boolean), y: (Long, Boolean)) => {
      val ((a, borderA), (b, borderB)) = (x, y)
      if (a != b) a compareTo b
      else if (borderA) -1
      else if (borderB) 1
      else 0
    })
    rangePoints.iterator.map(_._2) // safely return an iterator
  }
}
