package lessons._08.leader._1

object Solution {
  def solution(a: Array[Int]): Int = { // 100% | T: O(n) | S: O(1)
    val (size, candidate) = a.foldLeft((0, Option.empty[Int])) {
      case ((0, _), elem)                             => (1, Some(elem))
      case ((size, Some(mode)), elem) if mode == elem => (size + 1, Some(mode))
      case ((size, Some(mode)), _)                    => (size - 1, Some(mode))
    }
    candidate
      .filter(_ => size > 0)
      .filter(c => a.count(_ == c) > a.length / 2)
      .map(a.indexOf)
      .getOrElse(-1)
  }
}
