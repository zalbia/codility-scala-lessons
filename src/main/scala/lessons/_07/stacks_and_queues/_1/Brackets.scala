package lessons._07.stacks_and_queues._1

object Solution {
  // a List... can be a stack
  def solution(s: String): Int = // 100% O(n)
    if (balanced(s)) 1 else 0

  val openClose = Map('{' -> '}', '[' -> ']', '(' -> ')')

  // early exit
  private def balanced(s: String): Boolean = {
    var stack = List.empty[Char]
    for (bracket <- s) {
      if (openClose.contains(bracket))
        stack = bracket :: stack // push
      else {
        if (stack.isEmpty) return false
        val top = stack.head // peek
        if (bracket != openClose(top)) return false
        stack = stack.tail // pop
      }
    }
    stack.isEmpty
  }
}
