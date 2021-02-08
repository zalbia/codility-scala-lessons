object Solution {
  import scala.annotation.tailrec

  @tailrec
  def solution(a: Array[Int], k: Int): Array[Int] = { // 100%
    if (k == 0 || k == a.length || a.isEmpty) a
    else solution(a(a.length - 1) +: a.dropRight(1), k - 1)
  }
}
