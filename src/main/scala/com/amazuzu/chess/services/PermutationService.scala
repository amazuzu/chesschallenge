package com.amazuzu.chess.services

import com.amazuzu.chess._

/**
  * Created by taras on 9/1/16.
  */
class PermutationService {

  def permutationsDubl(r: Row): Traversable[Row] = r match {
    case h :: Nil => single(r)
    case h :: tail => _permutationsDubl(Nil, tail, h)
    case Nil => empty
  }

  private def _permutationsDubl(l: Row, r: Row, x: E): Traversable[Row] = {
    def helper = if (r.contains(x)) empty else permutationsDubl(l ::: r).map(s => x :: s)

    r match {
      case Nil => helper
      case h :: tail => helper ++: _permutationsDubl(l ::: (x :: Nil), tail, h)
    }

  }


  def permutations(r: Row): Traversable[Row] = r match {
    case h :: Nil => single(r)
    case h :: tail => _permutations(Nil, tail, h)
    case Nil => empty
  }


  private def _permutations(l: Row, r: Row, x: E): Traversable[Row] = {
    def helper = permutations(l ::: r).map(s => x :: s)

    r match {
      case Nil => helper
      case h :: tail => helper ++: _permutations(l ::: (x :: Nil), tail, h)
    }

  }

  val empty: Traversable[Row] = Stream.empty

  def single(el: Row): Traversable[Row] = el +: Stream.empty

}

class PermutationOnViewsService extends PermutationService {

  override val empty = new Traversable[Row] {
    override def foreach[U](f: (Row) => U): Unit = ()
  }.view

  override def single(r: Row) = new Traversable[Row] {
    override def foreach[U](f: (Row) => U): Unit = f(r)
  }.view

}