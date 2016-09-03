package com.amazuzu.chess.services

import com.amazuzu.chess.{Figure, _}
import org.specs2.mutable.Specification

/**
  * Created by taras on 9/3/16.
  */
trait FiguresCheck {
  self: Specification =>

  def expectNReturn(variants: Stream[Figures], n: Int): Set[Set[Figure]] = {
    variants.toList.length === n
    asSet(variants)
  }

  def asSet(variants: Stream[Figures]) = variants.toSet[Figures].map(_.toSet)
}
