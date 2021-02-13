package lessons._01.iterations._1

import Iterator.iterate

// N is an integer within the range [1..2,147,483,647].
object Solution {
  // iterate on bits from left to right
  // allocates 4 iterators and 1 range
  def solution(n: Int): Int = {
    var biggestGap, currentGap = 0
    val maxShift               = iterate(n)(_ >> 1).takeWhile(_ > 0).size - 1
    val bits                   = (maxShift to 0 by -1).toIterator.map(n >> _ & 1)
    bits.foreach {
      case 0 => currentGap += 1
      case 1 =>
        biggestGap = math.max(currentGap, biggestGap)
        currentGap = 0
    }
    biggestGap
  }
}

object Alternative1 {
  // iterate over right shifts, then check bits from right to left
  // only allocates 4 iterators
  def solution(n: Int): Int = { // 100% | O(log n) | O(1)
    var currentGap, biggestGap = 0
    val relevantBits           = iterate(n)(_ >> 1).takeWhile(_ > 0).map(_ & 1).dropWhile(_ != 1)
    relevantBits.foreach {
      case 0 =>
        currentGap += 1
        biggestGap = math.max(currentGap, biggestGap)
      case 1 =>
        currentGap = 0
    }
    biggestGap
  }
}

object Alternative2 {
  // allocates a O(log n) string
  def solution(n: Int): Int = { // 100% | time: O(log n) | space: O(log n)
    var biggestGap, currentGap = 0
    n.toBinaryString.foreach {
      case '0' => currentGap += 1
      case '1' =>
        biggestGap = math.max(currentGap, biggestGap)
        currentGap = 0
    }
    biggestGap
  }
}

object Alternative3 {
  // allocates short-lived tuples
  // allocates a O(log n) string
  def solution(n: Int): Int = // 100% | time: O(log n) | space: O(log n)
    n.toBinaryString
      .foldLeft((0, 0)) { case ((biggestGap, currentGap), bit) =>
        bit match {
          case '0' => (biggestGap, currentGap + 1)
          case '1' => (math.max(currentGap, biggestGap), 0)
        }
      }
      ._1
}
