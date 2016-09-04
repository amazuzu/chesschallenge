package com.amazuzu.chess.services

import com.amazuzu.chess.Row

/**
  * Created by taras on 9/1/16.
  */
case class ChessBoard(M: Int, N: Int, figures: Row) extends Printable {

  def variants = new PermutationService().permutationsDubl(figures).map(new ChessGameService(M, N, _).variants()).flatten

  def variantsView = new PermutationOnViewsService().permutationsDubl(figures).map(new ChessGameOnViewsService(M, N, _).variants()).flatten

  def runNDraw = variants.toList.foreach(print)

}

