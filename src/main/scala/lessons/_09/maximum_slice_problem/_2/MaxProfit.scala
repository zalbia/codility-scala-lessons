package lessons._09.maximum_slice_problem._2

object Solution {
  // keep track of lowest price and highest positive price difference (profit)
  def solution(a: Array[Int]): Int = {
    val (maxProfit, _) =
      a.foldLeft((0, Int.MaxValue)) { case ((maxProfit, minPrice), price) =>
        val newMinPrice  = math.min(minPrice, price)
        val newMaxProfit = math.max(maxProfit, price - minPrice)
        (newMaxProfit, newMinPrice)
      }
    maxProfit
  }
}
