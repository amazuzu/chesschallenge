package com.amazuzu.chess.services

import com.amazuzu.chess.{Point, _}

/**
  * Created by taras on 9/3/16.
  */
trait Printable {

  def N: Int

  def M: Int

  def print(figures: Figures) = {
    //println(figures.mkString(" "))
    println()
    for (y <- (N - 1 to(0, -1))) {
      println((0 until M).map(x =>
        figures.find(_.loc == Point(x, y)).map(" " + _.f.toString).getOrElse("  ")
      ).mkString(y.toString, "", ""))
    }
    println((0 until M).map(" " + _.toString).mkString(" ", "", ""))
  }
}

object Printable {
  def apply(_M: Int, _N: Int)(figures: Figures) = new Printable {
    val M = _M
    val N = _N

    print(figures)
  }
}
