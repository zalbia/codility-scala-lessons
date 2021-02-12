package lessons._05.prefix_sums._4

// N is an integer within the range [1..100,000];
// a in A : [0, 1]
object Solution {
  val max = 1000000000

  // early-exit
  def solution(a: Array[Int]): Int = { // 100% | O(n)
    var zeros = 0
    var passingCars = 0L
    for (elem <- a if passingCars <= max) {
      if (elem == 0)  zeros += 1
      else passingCars += zeros
    }
    if (passingCars > max) -1 else passingCars.toInt
  }
}
