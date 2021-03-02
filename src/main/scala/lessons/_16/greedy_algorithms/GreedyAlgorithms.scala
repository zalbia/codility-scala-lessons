package lessons._16.greedy_algorithms

import scala.collection.mutable

object GreedyAlgorithms {

  object coin_changing {

    object procedural_style {
      def greedyCoinChanging(m: Array[Int], k: Int) = {
        var amount = k
        val result = mutable.ArrayBuffer[(Int, Int)]()
        for (denomination <- m.reverseIterator) {
          result.append((denomination, amount / denomination))
          amount %= denomination
        }
        result.toArray
      }
    }

    object functional_style {
      def greedyCoinChanging(m: Array[Int], k: Int) =
        m.foldRight((k, List.empty[(Int, Int)])) { case (denomination, (amount, result)) =>
          (amount % denomination, (denomination, amount / denomination) :: result)
        }._2
          .toArray
          .reverse
    }
  }

  object greedy_canoeist {

    object greedy_algorithm {

      object procedural_style {
        // TODO: unfuck this, write a class or something
        def greedyCanoeist(w: Array[Int], k: Int) = { // T: O(n) amortized | S: O(n)
          // use mutable.ArrayBuffer as deques
          val skinnies = mutable.ArrayBuffer[Int]()
          val fatsos   = mutable.ArrayBuffer[Int]()
          for (i <- w.indices.dropRight(1))
            if (w(i) + w.last <= k)
              skinnies.append(w(i))
            else
              fatsos.append(w(i))
          fatsos.append(w.last)
          var canoes = 0
          while (skinnies.nonEmpty || fatsos.nonEmpty) {
            if (skinnies.nonEmpty)
              skinnies.remove(skinnies.length - 1)
            fatsos.remove(fatsos.length - 1)
            canoes += 1
            if (fatsos.isEmpty && skinnies.nonEmpty) {
              fatsos.append(skinnies.last)
              skinnies.remove(skinnies.length - 1)
            }
            while (fatsos.length > 1 && fatsos.last + fatsos.head <= k)
              skinnies.append(fatsos.remove(0))
          }
          canoes
        }
      }

      object functional_style {
        import lessons.collection.ListDeque

        import Iterator.iterate

        // TODO: what a fucking monstrosity, write tests and refactor this shit into case class logic
        def greedyCanoeist(w: Array[Int], k: Int): Int = { // T: O(n) amortized | S: O(n)
          // divide canoeists into skinnies and fatsos
          val (skinnies, fatsos) =
            w.indices.dropRight(1).foldLeft((ListDeque.empty[Int], ListDeque.empty[Int])) {
              case ((skinnies, fatsos), i) =>
                if (w(i) + w.last <= k) (skinnies.pushRight(w(i)), fatsos)
                else (skinnies, fatsos.pushRight(w(i)))
            }
          // pair skinnies and fatsos
          val start              = (skinnies, fatsos.pushRight(w.last), 0)
          val (_, _, canoes)     = iterate(start) { case (skinnies, fatsos, canoes) =>
            val skinnies1            = skinnies.popRight._2
            val fatsos1              = fatsos.popRight._2
            val canoes1              = canoes + 1
            val (skinnies2, fatsos2) = skinnies1.peekRight
              .filter(_ => fatsos1.isEmpty)
              .map(last => (skinnies1.popRight._2, fatsos1.pushRight(last)))
              .getOrElse((skinnies1, fatsos1))
            val start                = (skinnies2, fatsos2.balanced)
            val (skinnies3, fatsos3) = iterate(start) { case (skinnies, fatsos) =>
              val (fatso, fatsos1) = fatsos.popLeft
              val skinnies1        = fatso.map(skinnies.pushRight).getOrElse(skinnies)
              (skinnies1, fatsos1)
            }.takeWhile { case (_, fatsos) =>
              fatsos.size > 1 && (fatsos.peekRight zip fatsos.peekLeft).exists { case (a, b) => a + b <= k }
            }.fold(start)((_, next) => next)
            (skinnies3, fatsos3, canoes1)
          }.takeWhile { case (skinnies, fatsos, _) => skinnies.nonEmpty || fatsos.nonEmpty }
            .reduce((_, next) => next)
          canoes
        }
      }
    }

    object greedy_canoeist_shorter {

      object procedural_style {
        def greedyCanoeist(w: Array[Int], k: Int) = {
          var canoes = 0
          var j      = 0
          var i      = w.length - 1
          while (i >= j) {
            if (w(i) + w(j) <= k)
              j += 1
            canoes += 1
            i -= 1
          }
          canoes
        }
      }

      object functional_style {
        def greedyCanoeist(w: Array[Int], k: Int) = {
          val (_, _, canoes) = Iterator
            .iterate((0, 0, w.length - 1)) { case (canoes, j, i) =>
              (canoes + 1, if (w(i) + w(j) <= k) j + 1 else j, i - 1)
            }
            .takeWhile { case (_, j, i) => i >= j }
            .reduce((_, next) => next)
          canoes
        }
      }
    }
  }
}
