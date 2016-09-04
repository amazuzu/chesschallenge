package com.amazuzu.chess.services

import com.amazuzu.chess._

import scala.Stream._

/**
  * Created by taras on 9/2/16.
  */
class ChessGameService(val M: Int, val N: Int, figures: Row) extends Printable {

  case class GameUncompleted(resolved: Figures, rest: Row) extends Game {

    override def variants(): Flow = rest match {
      case h :: Nil => runSafelyThroughDesk(h)(p => singleFlow(Figure(p, h) :: resolved))
      case h :: tail => runSafelyThroughDesk(h)(p => GameUncompleted(Figure(p, h) :: resolved, tail).variants())
      case Nil => singleFlow(resolved)
    }

  }

  case class GameCompleted(resolved: Figures) extends Game {

    override def variants(): Flow = runThroughDesk(resolved.head.loc)(p =>
      GameUncompleted(Figure(p, figures.head) :: Nil, figures.tail).variants()
    )
  }


  def variants() = if (figures.isEmpty) emptyFlow
  else GameCompleted(figures.map(Figure(Point.BeforeZero, _))).variants()


  sealed trait Game {

    def variants(): Flow

    def resolved: Figures

    protected def runThroughDesk(startPoint: Point)(closure: Point => Flow) = {
      def next(cursor: Point): Flow =
        if (isValid(cursor)) closure(cursor) ++: next(nextStep(cursor)) else emptyFlow

      next(nextStep(startPoint))
    }


    protected def runSafelyThroughDesk(newFigure: E)(closure: Point => Flow) = {
      def next(ocursor: Option[Point]): Flow = ocursor match {
        case Some(cursor) => closure(cursor) ++: next(findNextSafeLocation(cursor, newFigure))
        case _ => emptyFlow
      }

      next(findNextSafeLocation(resolved.head.loc, newFigure))
    }

    protected def findNextSafeLocation(startPoint: Point, newFigure: E) = {
      var p = nextStep(startPoint)
      while (isValid(p) && !isSafeLocation(p, newFigure)) p = nextStep(p)
      if (isValid(p)) Some(p) else None
    }

    private def isMutuallySafe(a: Point, f: E, b: Point, g: E) = {
      val dx = Math.abs(a.x - b.x)
      val dy = Math.abs(a.y - b.y)

      def safe(el: E) = el match {
        case 'N' => dx > 1 || dy > 1
        case 'Q' => a.x != b.x && a.y != b.y && dx != dy
        case 'B' => dx != dy
        case 'R' => a.x != b.x && a.y != b.y
        case 'K' => !(dx == 1 && dy == 2 || dx == 2 && dy == 1)
      }

      safe(f) && safe(g)
    }

    protected def isSafeLocation(p: Point, e: E) = resolved.forall(o => isMutuallySafe(p, e, o.loc, o.f))

    protected def nextStep(p: Point) = if (p.x + 1 >= M) p.copy(x = 0, y = p.y + 1) else p.copy(x = p.x + 1)

    protected def isValid(p: Point) = p.y < N

  }

  val emptyFlow: Flow = empty

  def singleFlow(el: Figures): Flow = el +: empty

}
