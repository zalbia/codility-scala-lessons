object Solution {
  def solution(a: Array[Int]) = // 100%, O(n log n), can be O(n) with set
    if (a.length <= 1) a.length
    else a.sorted.iterator.sliding(2).count(l => l.exists(_ != l.head)) + 1
}

import scala.util.Random

val random = new Random()
val f = Solution.solution _

f(Array(2, 1, 1, 2, 3, 1)) // 3
f(Array(-1000000, 1000000)) // 2
f(Array.iterate(0, 100000)(_ + 1)) // 100000
f(Array.iterate(-1000000, 100000)(_ + 20)) // 100000
f(Array.fill(100000)(1000000)) // 1
f(Array.fill(100000)(random.nextInt(2000000) - 1000000))
