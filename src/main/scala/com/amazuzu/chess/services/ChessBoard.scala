package com.amazuzu.chess.services

import com.amazuzu.chess.{Figures, Flow, Row}
import PermutationService._
import com.amazuzu.chess.services.view.PermutationViewService

/**
  * Created by taras on 9/1/16.
  */
case class ChessBoard(M: Int, N: Int, figures: Row) extends Printable {

  def variants = permutationsDubl(figures).map(perm =>
    new ChessGameService(M, N, perm).variants()
  ).flatten

  def variantsView = PermutationViewService.permutationsDublView(figures).map(new ChessGameService(M, N, _) {

    override val emptyFlow: Flow = new Traversable[Figures] {
      override def foreach[U](f: (Figures) => U): Unit = ()
    }.view

    override def singleFlow(el: Figures): Flow = new Traversable[Figures] {
      override def foreach[U](f: (Figures) => U): Unit = f(el)
    }.view

  }.variants()
  ).flatten

  def runNDraw = variants.toList.foreach { result =>
    print(result)
  }

}

