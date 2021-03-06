package com.amazuzu

/**
  * Created by taras on 9/1/16.
  */
package object chess {
  type E = Char
  type Row = List[E]
  type Bracket = (Row, E, Row)

  case class Point(x: Int, y: Int) {
    override def toString: String = s"($x,$y)"
  }

  object Point {
    val Zero = new Point(0, 0)
    val BeforeZero = new Point(-1, 0) //the point before first: Zero = nextStep(BeforeZero)
  }

  case class Figure(loc: Point, f: E)

  type Figures = List[Figure]

  type Flow = Traversable[Figures]


}
