package com.amazuzu.chess.services

import com.amazuzu.chess.{Figure, Point}
import org.specs2.mutable.Specification

/**
  * Created by taras on 9/2/16.
  */
class ChessGameSpec extends Specification {
  "chess game service" should {

    "give 1 result for 1x1 desk and 1 figure" in {
      ChessGameService(1, 1, List('N')).variants().toList === List(Figure(Point.Zero, 'N')) :: Nil
    }

    "give 0 result for 1x1 desk and 0 figures" in {
      ChessGameService(1, 1, List()).variants().toList === List(Nil)
    }

    "give 2 result for 3x2 desk and 2 kings" in {
      val chess = ChessGameService(3, 2, List('N', 'Q'))
      val result = chess.variants().toSet


      result.map(chess.draw)
      //chess.draw(result.head)

       result === Set(
        List(Figure(Point(2, 1), 'Q'), Figure(Point(0, 0), 'N')),
        List(Figure(Point(0, 1), 'Q'), Figure(Point(2, 0), 'N'))
      )

    }
  }
}
