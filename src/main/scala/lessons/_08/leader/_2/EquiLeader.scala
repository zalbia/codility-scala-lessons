package lessons._08.leader._2

object Solution {
  // N is an integer within the range [1..100,000];
  // each element of array A is an integer within the range [−1,000,000,000..1,000,000,000].
  def solution(a: Array[Int]): Int = // 100% | T: O(n) | S: O(n)
    leader(a).map { leader =>
      val leftCounts  = a.scanLeft(0)(count(leader))
      val rightCounts = a.reverseIterator.scanLeft(0)(count(leader)).toArray
      leftCounts.indices.iterator.map { s =>
        val sLeft      = s
        val sRight     = a.length - s
        val leftCount  = leftCounts(sLeft)
        val rightCount = rightCounts(sRight)
        val leftIsLeader = leftCount > sLeft / 2
        val rightIsLeader = rightCount > sRight / 2
        leftIsLeader && rightIsLeader
      }.count(identity)
    }.getOrElse(0)

  def count(leader: Int)(count: Int, elem: Int): Int =
    if (elem == leader) count + 1 else count

  def leader(a: Array[Int]): Option[Int] = { // T: O(n) | S: O(1)
    val (size, candidate) = a.foldLeft((0, Option.empty[Int])) {
      case ((0, _), elem)                             => (1, Some(elem))
      case ((size, Some(mode)), elem) if mode == elem => (size + 1, Some(mode))
      case ((size, Some(mode)), _)                    => (size - 1, Some(mode))
    }
    candidate
      .filter(_ => size > 0)
      .filter(c => a.count(_ == c) > a.length / 2)
  }
}
