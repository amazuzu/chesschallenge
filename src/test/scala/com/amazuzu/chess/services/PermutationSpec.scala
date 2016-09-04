package com.amazuzu.chess.services

import org.specs2.mutable.Specification

/**
  * Created by taras on 9/1/16.
  */
class PermutationSpec extends Specification {

  val service = new PermutationService()
  val serviceView = new PermutationOnViewsService()

  "permutation service" should {
    "return Nil if input list is empty" in {

      service.permutations(List()).toList === Nil
      serviceView.permutationsDubl(List()).toList === Nil
    }

    "return proper result for regular permutations" in {
      service.permutationsDubl(List('A', 'B', 'C')).toSet === Set(
        List('A', 'B', 'C'), List('A', 'C', 'B'), List('B', 'A', 'C'),
        List('B', 'C', 'A'), List('C', 'A', 'B'), List('C', 'B', 'A'))

      serviceView.permutationsDubl(List('A', 'B', 'C')).toSet === Set(
        List('A', 'B', 'C'), List('A', 'C', 'B'), List('B', 'A', 'C'),
        List('B', 'C', 'A'), List('C', 'A', 'B'), List('C', 'B', 'A'))
    }

    "return 1 permutation from single figure list" in {
      service.permutationsDubl(List('A', 'A', 'A', 'A')).toList === List(List('A', 'A', 'A', 'A'))

      serviceView.permutationsDubl(List('A', 'A', 'A', 'A')).toList === List(List('A', 'A', 'A', 'A'))
    }

    "return unique elements if no dublicates" in {
      val stream0 = service.permutations(List('A', 'B', 'C', 'D', 'E', 'F'))
      val list0 = stream0.toList

      val stream1 = service.permutationsDubl(List('A', 'B', 'C', 'D', 'E', 'F'))
      val list1 = stream1.toList

      list0.length === list0.toSet.size
      list1.length === list1.toSet.size

      list0.toSet === list1.toSet
    }

    "return unique elements if no dublicates for views" in {
      val stream0 = serviceView.permutations(List('A', 'B', 'C', 'D', 'E', 'F'))
      val list0 = stream0.toList

      val stream1 = serviceView.permutationsDubl(List('A', 'B', 'C', 'D', 'E', 'F'))
      val list1 = stream1.toList

      list0.length === list0.toSet.size
      list1.length === list1.toSet.size

      list0.toSet === list1.toSet
    }

    "return unique elements if dublicates exists" in {
      val stream = service.permutationsDubl(List('A', 'B', 'C', 'D', 'C', 'C', 'A', 'D', 'D'))
      val list = stream.toList

      list.length === list.toSet.size
    }

    "return unique elements if dublicates exists 2" in {
      val stream = service.permutationsDubl(List('N', 'N', 'Q', 'Q', 'B', 'B', 'K'))
      val list = stream.toList

      list.length === list.toSet.size
    }
  }
}
