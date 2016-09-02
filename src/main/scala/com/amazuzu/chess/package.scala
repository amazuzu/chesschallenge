package com.amazuzu

/**
  * Created by taras on 9/1/16.
  */
package object chess {
  type E = Char
  type Row = List[E]
  type Bracket = (Row, E, Row)

  class Point(var x: Int, var y: Int)

  case class Figure(x: Int, y: Int, f: E)

  type Figures = List[Figure]
}
