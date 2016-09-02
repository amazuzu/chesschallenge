package com.amazuzu.chess.services

import com.amazuzu.chess._
import Stream._

/**
  * Created by taras on 9/2/16.
  */
case class ChessGameService(M: Int, N: Int, figures: Row) {

  case class GameUncompleted(val resolved: Figures, rest: Row) extends Game {

    override def variants(): Stream[Figures] = rest match {
      case h :: Nil => runSafelyThroughDesk(h) { p =>
        val game = GameCompleted(Figure(p, h) :: resolved)
        game.resolved #:: game.variants()
      }

      case h :: tail => runSafelyThroughDesk(h) { p =>
        GameUncompleted(Figure(p, h) :: resolved, tail).variants()
      }

      case Nil => resolved #:: empty
    }

  }

  case class GameCompleted(val resolved: Figures) extends Game {

    override def variants(): Stream[Figures] = runThroughDesk(resolved.last.loc)(p =>
      GameUncompleted(Figure(p, figures.head) :: Nil, figures.tail).variants()
    )
  }


  def variants(): Stream[Figures] = if (figures.isEmpty) Nil #:: empty
  else GameCompleted(figures.map(Figure(Point.BeforeZero, _))).variants()


  sealed trait Game {

    def variants(): Stream[Figures]

    def resolved: Figures

    def runThroughDesk(startPoint: Point)(closure: Point => Stream[Figures]) = {
      var cursor = startPoint

      Stream.continually {
        cursor = nextStep(cursor)
        cursor
      }.takeWhile(isValid(_)).map(closure(_)).flatten
    }


    def runSafelyThroughDesk(newFigure: E)(closure: Point => Stream[Figures]): Stream[Figures] = {
      var ocursor: Option[Point] = Some(resolved.head.loc)

      Stream.continually {
        ocursor = findNextSafeLocation(ocursor.get, newFigure)
        ocursor
      }.takeWhile(_.isDefined).map(oc => closure(oc.get)).flatten
    }

    protected def findNextSafeLocation(startPoint: Point, newFigure: E): Option[Point] = {
      var p = nextStep(startPoint)

      while (isValid(p) && !isSafeLocation(p, Figure(p, newFigure))) p = nextStep(p)

      if (isValid(p)) Some(p) else None
    }


    private def isSafePointAgainstFigure(p: Point, f: Figure) = f match {
      case Figure(Point(x, y), 'N') => Math.abs(x - p.x) > 1 || Math.abs(y - p.y) > 1
      case Figure(Point(x, y), 'Q') => x != p.x && y != p.y && Math.abs(x - p.x) != Math.abs(y - p.y)
      case Figure(Point(x, y), 'B') => Math.abs(x - p.x) != Math.abs(y - p.y)
      case Figure(Point(x, y), 'R') => x != p.x && y != p.y
      case Figure(Point(x, y), 'K') => !(Math.abs(x - p.x) == 2 && Math.abs(y - p.y) == 3 || Math.abs(x - p.x) == 3 && Math.abs(y - p.y) == 2)
    }

    protected def isSafeLocation(p: Point, g: Figure): Boolean = resolved.forall(f => isSafePointAgainstFigure(p, f) && isSafePointAgainstFigure(f.loc, g))

    protected def nextStep(p: Point): Point = {
      val clonep = p.copy(x = p.x + 1)

      if (clonep.x >= M) clonep.copy(x = 0, y = clonep.y + 1) else clonep
    }

    protected def isValid(p: Point) = p.y < N

  }


  def draw(figures: Figures) = {
    println(figures.mkString(" "))
    for (y <- (N - 1 to(0, -1))) {
      println((0 until M).map(x =>
        figures.find(_.loc == Point(x, y)).map(" " + _.f.toString).getOrElse("  ")
      ).mkString(y.toString, "", ""))
    }
    println((0 until M).map(" " + _.toString).mkString(" ", "", ""))
  }

}
