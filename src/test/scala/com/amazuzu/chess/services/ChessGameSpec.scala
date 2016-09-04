package com.amazuzu.chess.services

import com.amazuzu.chess.{Figure, Point}
import org.specs2.mutable.Specification

/**
  * Created by taras on 9/2/16.
  */
class ChessGameSpec extends Specification with FiguresCheck {
  "chess game service" should {

    "give 1 result for 1x1 desk and 1 figure" in {
      new ChessGameService(1, 1, List('N')).variants().toList === List(Figure(Point.Zero, 'N')) :: Nil

      new ChessGameOnViewsService(1, 1, List('N')).variants().toList === List(Figure(Point.Zero, 'N')) :: Nil
    }

    "give 0 result for 1x1 desk and 0 figures" in {
      new ChessGameService(1, 1, List()).variants().toList === List()

      new ChessGameOnViewsService(1, 1, List()).variants().toList === List()
    }

    "give 2 results for 3x2 desk and 2 kings" in {
      val chess = new ChessGameService(3, 2, List('N', 'Q'))

      expectNReturn(chess.variants(), 2) === Set(
        Set(Figure(Point(2, 1), 'Q'), Figure(Point(0, 0), 'N')),
        Set(Figure(Point(0, 1), 'Q'), Figure(Point(2, 0), 'N'))
      )

      val chessViews = new ChessGameOnViewsService(3, 2, List('N', 'Q'))

      expectNReturn(chessViews.variants(), 2) === Set(
        Set(Figure(Point(2, 1), 'Q'), Figure(Point(0, 0), 'N')),
        Set(Figure(Point(0, 1), 'Q'), Figure(Point(2, 0), 'N'))
      )
    }

    "give 4 results for 3x3 desk, 2 kings and 1 rook, case N R N" in {
      val chess = new ChessGameService(3, 3, List('N', 'R', 'N'))

      expectNReturn(chess.variants(), 2) === Set(
        Set(Figure(Point(0, 0), 'N'), Figure(Point(2, 1), 'R'), Figure(Point(0, 2), 'N')),
        Set(Figure(Point(2, 0), 'N'), Figure(Point(0, 1), 'R'), Figure(Point(2, 2), 'N'))
      )

      val chessViews = new ChessGameOnViewsService(3, 3, List('N', 'R', 'N'))

      expectNReturn(chessViews.variants(), 2) === Set(
        Set(Figure(Point(0, 0), 'N'), Figure(Point(2, 1), 'R'), Figure(Point(0, 2), 'N')),
        Set(Figure(Point(2, 0), 'N'), Figure(Point(0, 1), 'R'), Figure(Point(2, 2), 'N'))
      )
    }

    "give 4 results for 3x3 desk, 2 kings and 1 rook, case R N N" in {
      val chess = new ChessGameService(3, 3, List('R', 'N', 'N'))

      expectNReturn(chess.variants(), 1) === Set(
        Set(Figure(Point(1, 0), 'R'), Figure(Point(0, 2), 'N'), Figure(Point(2, 2), 'N'))
      )

      val chessViews = new ChessGameOnViewsService(3, 3, List('R', 'N', 'N'))

      expectNReturn(chessViews.variants(), 1) === Set(
        Set(Figure(Point(1, 0), 'R'), Figure(Point(0, 2), 'N'), Figure(Point(2, 2), 'N'))
      )
    }

    "give 4 results for 3x3 desk, 2 kings and 1 rook, case N N R" in {
      val chess = new ChessGameService(3, 3, List('N', 'N', 'R'))

      expectNReturn(chess.variants(), 1) === Set(
        Set(Figure(Point(1, 2), 'R'), Figure(Point(2, 0), 'N'), Figure(Point(0, 0), 'N'))
      )

      val chessViews = new ChessGameOnViewsService(3, 3, List('N', 'N', 'R'))

      expectNReturn(chessViews.variants(), 1) === Set(
        Set(Figure(Point(1, 2), 'R'), Figure(Point(2, 0), 'N'), Figure(Point(0, 0), 'N'))
      )
    }

    "give 3 results for 4x1 desk, 2 kings" in {
      val chess = new ChessGameService(4, 1, List('N', 'N'))

      expectNReturn(chess.variants(), 3) === Set(
        Set(Figure(Point(0, 0), 'N'), Figure(Point(2, 0), 'N')),
        Set(Figure(Point(0, 0), 'N'), Figure(Point(3, 0), 'N')),
        Set(Figure(Point(1, 0), 'N'), Figure(Point(3, 0), 'N'))
      )

      val chessViews = new ChessGameOnViewsService(4, 1, List('N', 'N'))

      expectNReturn(chessViews.variants(), 3) === Set(
        Set(Figure(Point(0, 0), 'N'), Figure(Point(2, 0), 'N')),
        Set(Figure(Point(0, 0), 'N'), Figure(Point(3, 0), 'N')),
        Set(Figure(Point(1, 0), 'N'), Figure(Point(3, 0), 'N'))
      )
    }

    "give correct results on 3x2 and 2 kings case" in {
      val chess = new ChessGameService(3, 2, List('N', 'N'))
      chess.variants().toList.groupBy(identity).collect { case (x, List(_, _, _*)) => x }.isEmpty

      val chessViews = new ChessGameOnViewsService(3, 2, List('N', 'N'))
      chessViews.variants().toList.groupBy(identity).collect { case (x, List(_, _, _*)) => x }.isEmpty
    }

  }
}
