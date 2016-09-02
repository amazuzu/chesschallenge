package com.amazuzu.chess.services

import com.amazuzu.chess.Row
import PermutationService._

/**
  * Created by taras on 9/1/16.
  */
case class ChessBoard(M: Int, N: Int, figures: Row) {

  val perms = permutationsDubl(figures)

  def game = perms.map { figures =>

      //ConcreteGame(M, N, figures).variants

  }

}
