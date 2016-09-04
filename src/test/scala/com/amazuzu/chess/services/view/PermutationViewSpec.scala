package com.amazuzu.chess.services.view

import org.specs2.mutable.Specification
import PermutationViewService._

/**
  * Created by taras on 9/1/16.
  */
class PermutationViewSpec extends Specification {

  "permutation views service" should {
    "return Nil if input list is empty" in {

      permutationsView(List()).toList === List()
    }

    "return proper result for regular permutations" in {
      permutationsView(List('A', 'B', 'C')).toSet === Set(
        List('A', 'B', 'C'), List('A', 'C', 'B'), List('B', 'A', 'C'),
        List('B', 'C', 'A'), List('C', 'A', 'B'), List('C', 'B', 'A'))
    }

    "return 1 permutation from single figure list" in {
      permutationsDublView(List('A', 'A', 'A', 'A')).toList === List(List('A', 'A', 'A', 'A'))
    }

    "return unique elements if no dublicates" in {
      val stream0 = permutationsView(List('A', 'B', 'C', 'D', 'E', 'F'))
      val list0 = stream0.toList

      val stream1 = permutationsDublView(List('A', 'B', 'C', 'D', 'E', 'F'))
      val list1 = stream1.toList

      list0.length === list0.toSet.size
      list1.length === list1.toSet.size

      list0.toSet === list1.toSet
    }

    "return unique elements if dublicates exists" in {
      val stream = permutationsDublView(List('A', 'B', 'C', 'D', 'C', 'C', 'A', 'D', 'D'))
      val list = stream.toList

      list.length === list.toSet.size
    }

    "return unique elements if dublicates exists 2" in {
      val stream = permutationsDublView(List('N', 'N', 'Q', 'Q', 'B', 'B', 'K'))
      val list = stream.toList

      list.length === list.toSet.size
    }
  }
}
