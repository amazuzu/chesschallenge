package com.amazuzu.chess.services.view

import com.amazuzu.chess._

/**
  * Created by taras on 9/4/16.
  */
object PermutationViewService {

  def permutationsDublView(r: Row): Traversable[Row] = r match {
    case h :: Nil => SingleTraversable(r)

    case h :: tail => _permutationsDublView(Nil, tail, h)
    case Nil => EmptyTraversable
  }


  private def _permutationsDublView(l: Row, r: Row, x: E): Traversable[Row] = {
    def helper = if (r.contains(x)) EmptyTraversable else permutationsDublView(l ::: r).map(s => x :: s)

    r match {
      case Nil => helper
      case h :: tail => helper ++: _permutationsDublView(l ::: (x :: Nil), tail, h)
    }

  }


  def permutationsView(r: Row): Traversable[Row] = r match {
    case h :: Nil => SingleTraversable(r)

    case h :: tail => _permutationsView(Nil, tail, h)
    case Nil => EmptyTraversable
  }


  private def _permutationsView(l: Row, r: Row, x: E): Traversable[Row] = {
    def helper = permutationsView(l ::: r).map(s => x :: s)

    r match {
      case Nil => helper
      case h :: tail => helper ++: _permutationsView(l ::: (x :: Nil), tail, h)
    }

  }


  val EmptyTraversable = new Traversable[Row] {
    override def foreach[U](f: (Row) => U): Unit = ()
  }.view

  def SingleTraversable(r: Row) = new Traversable[Row] {
    override def foreach[U](f: (Row) => U): Unit = f(r)
  }.view

}
