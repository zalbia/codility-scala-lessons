package lessons._07.stacks_and_queues._4

object Solution {
  // N is an integer within the range [1..100,000];
  // each element of array H is an integer within the range [1..1,000,000,000].
  def solution(h: Array[Int]): Int = { // O(n) | 100%
    var blockCount = 0
    var stack      = List.empty[Int]
    for (height <- h) {
      stack = stack.dropWhile(_ > height)
      stack match {
        case top :: _ if top == height => ()
        case _                         =>
          blockCount += 1
          stack = height :: stack
      }
    }
    blockCount
  }
}
