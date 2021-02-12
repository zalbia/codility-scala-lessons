package lessons._07.stacks_and_queues._3

object Solution {
  // N is an integer within the range [0..1,000,000];
  // string S consists only of the characters "(" and/or ")".
  def solution(s: String): Int =
    if (s.length % 2 == 1) 0 else checkNesting(s)

  // yet another early exit problem
  private def checkNesting(s: String): Int = {
    var openCount = 0
    for (i <- s.indices) {
      val paren = s(i)
      if (paren == '(') openCount += 1
      else if (openCount == 0) return 0
      else openCount -= 1
    }
    if (openCount == 0) 1 else 0
  }
}
