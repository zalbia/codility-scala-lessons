import scala.collection.JavaConverters._

object Solution {
  def solution = ???
}

import scala.util.Random
import scala.util.{Failure, Success}
import utest._

val random = new Random()
val f = Solution.solution _

TestRunner.run(Tests {
  test("ProblemName") {
    f
  }
}).leaves.map(_.value).foreach {
  case Failure(exception) => println(exception)
  case Success(value) => println(value)
}
