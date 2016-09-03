package com.amazuzu.chess.services

import com.amazuzu.chess.{Figure, Point}
import org.specs2.mutable.Specification

/**
  * Created by taras on 9/3/16.
  */
class ChessBoardSpec extends Specification with FiguresCheck {
  "chess board" should {

    "give correct result for 3x3 board, 2 kings and 1 rook" in {
      val board = ChessBoard(3, 3, List('N', 'N', 'R'))
      val variants = board.variants

      val v0 = Set(Figure(Point(0, 0), 'N'), Figure(Point(2, 1), 'R'), Figure(Point(0, 2), 'N'))
      val v1 = Set(Figure(Point(2, 0), 'N'), Figure(Point(0, 1), 'R'), Figure(Point(2, 2), 'N'))

      val v2 = Set(Figure(Point(1, 0), 'R'), Figure(Point(0, 2), 'N'), Figure(Point(2, 2), 'N'))
      val v3 = Set(Figure(Point(1, 2), 'R'), Figure(Point(2, 0), 'N'), Figure(Point(0, 0), 'N'))

      expectNReturn(variants, 4) === Set(v0, v1, v2, v3)

    }


    "give correct result for 4x4 board, 2 rooks and 4 knights" in {
      val board = ChessBoard(4, 4, List('R', 'R', 'K', 'K', 'K', 'K'))
      val variants = board.variants

      val v0 = Set(Figure(Point(0, 0), 'R'), Figure(Point(1, 1), 'K'), Figure(Point(3, 1), 'K'), Figure(Point(2, 2), 'R'), Figure(Point(1, 3), 'K'), Figure(Point(3, 3), 'K'))
      val v1 = Set(Figure(Point(2, 0), 'R'), Figure(Point(1, 1), 'K'), Figure(Point(3, 1), 'K'), Figure(Point(0, 2), 'R'), Figure(Point(1, 3), 'K'), Figure(Point(3, 3), 'K'))

      val v2 = Set(Figure(Point(1, 0), 'K'), Figure(Point(3, 0), 'K'), Figure(Point(2, 1), 'R'), Figure(Point(1, 2), 'K'), Figure(Point(3, 2), 'K'), Figure(Point(0, 3), 'R'))
      val v3 = Set(Figure(Point(1, 0), 'K'), Figure(Point(3, 0), 'K'), Figure(Point(0, 1), 'R'), Figure(Point(1, 2), 'K'), Figure(Point(3, 2), 'K'), Figure(Point(2, 3), 'R'))

      val v4 = Set(Figure(Point(0, 0), 'K'), Figure(Point(2, 0), 'K'), Figure(Point(3, 1), 'R'), Figure(Point(0, 2), 'K'), Figure(Point(2, 2), 'K'), Figure(Point(1, 3), 'R'))
      val v5 = Set(Figure(Point(0, 0), 'K'), Figure(Point(2, 0), 'K'), Figure(Point(1, 1), 'R'), Figure(Point(0, 2), 'K'), Figure(Point(2, 2), 'K'), Figure(Point(3, 3), 'R'))

      val v6 = Set(Figure(Point(1, 0), 'R'), Figure(Point(0, 1), 'K'), Figure(Point(2, 1), 'K'), Figure(Point(3, 2), 'R'), Figure(Point(0, 3), 'K'), Figure(Point(2, 3), 'K'))
      val v7 = Set(Figure(Point(3, 0), 'R'), Figure(Point(0, 1), 'K'), Figure(Point(2, 1), 'K'), Figure(Point(1, 2), 'R'), Figure(Point(0, 3), 'K'), Figure(Point(2, 3), 'K'))

      expectNReturn(variants, 8) === Set(v0, v1, v2, v3, v4, v5, v6, v7)
    }
  }
}
