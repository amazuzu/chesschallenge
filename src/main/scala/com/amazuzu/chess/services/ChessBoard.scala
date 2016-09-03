package com.amazuzu.chess.services

import com.amazuzu.chess.Row
import PermutationService._

/**
  * Created by taras on 9/1/16.
  */
case class ChessBoard(M: Int, N: Int, figures: Row) extends Printable {

  def variants = permutationsDubl(figures).map(perm =>
    ChessGameService(M, N, perm).variants()
  ).flatten

  def runNDraw = variants.toList.foreach { result =>
    print(result)
  }

}

object ChessApp extends App {
  ChessBoard(4, 4, List('R', 'R', 'K', 'K', 'K', 'K')).runNDraw
}