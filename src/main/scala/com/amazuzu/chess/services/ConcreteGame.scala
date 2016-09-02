package com.amazuzu.chess.services

import com.amazuzu.chess._


/**
  * Created by taras on 9/2/16.
  */
case class Game(M: Int, N: Int, figures: Row) {
  def variants(): Stream[Figures] = GameInstance(M, N, Nil, figures).variants()
}

case class GameInstance(M: Int, N: Int, stay: Figures, rest: Row) {

  import Stream._

  def variants(): Stream[Figures] = this match {

    //build first chain element
    case GameInstance(_, _, Nil, _) => copy(stay = Figure(0, 0, rest.head) :: Nil, rest = rest.tail).variants()
    //build next chain

    //todo move partial positions
    case GameInstance(_, _, _, fh :: ftail) => findNextSafeLocation() match {
      case Some(p) => copy(stay = Figure(p.x, p.y, fh) :: stay, rest = ftail).variants()
      case _ => empty
    }

    //todo move final position, may be should be handled by code from above
    //move completed figure
    //case GameInstance(_, _, _, Nil)

  }

  def findNextSafeLocation(): Option[Point] = {
    val p = nextStep(new Point(stay.head.x, stay.head.y))

    while (p.y < M && !isSafeLocation(p)) nextStep(p)

    if (p.y < M) Some(p) else None
  }


  private def isSafePointForFigure(p: Point, f: Figure) = f match {
    case Figure(x, y, 'N') => Math.abs(x - p.x) > 1 || Math.abs(y - p.y) > 1
    case Figure(x, y, 'Q') => x != p.x && y != p.y && Math.abs(x - p.x) != Math.abs(y - p.y)
    case Figure(x, y, 'B') => Math.abs(x - p.x) != Math.abs(y - p.y)
    case Figure(x, y, 'R') => x != p.x && y != p.y
    case Figure(x, y, 'K') => !(Math.abs(x - p.x) == 2 && Math.abs(y - p.y) == 3 || Math.abs(x - p.x) == 3 && Math.abs(y - p.y) == 2)
  }

  private def isSafeLocation(p: Point): Boolean = stay.forall(f => isSafePointForFigure(p, f))

  private def nextStep(p: Point): Point = {
    p.x += 1

    if (p.x > N) {
      p.x = 0
      p.y += 1
    }

    p

  }

  //private def isValid(p: Point) = p.y < M
}
