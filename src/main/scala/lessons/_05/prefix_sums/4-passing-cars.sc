object Solution {
  // N is an integer within the range [1..100,000];
  // a in A : [0, 1]
  def solution(a: Array[Int]): Int = { // 100%
    var zeros = 0
    var passingCars = 0L
    a.foreach {
      case 0 => zeros += 1
      case 1 => passingCars += zeros
    }
    if (passingCars > 1000000000) -1 else passingCars.toInt
  }
}

import scala.util.Random
val random = new Random()
val f = Solution.solution _

f(Array(0,1,0,1,1))
f(Array.fill(100000)(random.nextInt(2)))
f(Array.iterate(1, 100000)(n => if (n == 0) 1 else 0))