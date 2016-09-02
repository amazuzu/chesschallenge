package com.amazuzu.chess.services

import com.amazuzu.chess._

/**
  * Created by taras on 9/1/16.
  */
object PermutationService {

  import Stream._

  def permutationsDubl(r: Row): Stream[Row] = r match {
    case h :: Nil => r #:: empty
    case h :: tail => _permutationsDubl(Nil, tail, h)
    case Nil => empty
  }

  private def _permutationsDubl(l: Row, r: Row, x: E): Stream[Row] = {
    def helper = if (r.contains(x)) empty else permutationsDubl(l ::: r).map(s => x :: s)

    r match {
      case Nil => helper
      case h :: tail => helper #::: _permutationsDubl(l ::: (x :: Nil), tail, h)
    }

  }


  def permutations(r: Row): Stream[Row] = r match {
    case h :: Nil => r #:: empty
    case h :: tail => _permutations(Nil, tail, h)
    case Nil => empty
  }


  private def _permutations(l: Row, r: Row, x: E): Stream[Row] = {
    def helper = permutations(l ::: r).map(s => x :: s)

    r match {
      case Nil => helper
      case h :: tail => helper #::: _permutations(l ::: (x :: Nil), tail, h)
    }

  }

}
