package com.amazuzu.chess.services

import com.amazuzu.chess.{Figure, _}
import org.specs2.mutable.Specification

/**
  * Created by taras on 9/3/16.
  */
trait FiguresCheck {
  self: Specification =>

  def expectNReturn(variants: Flow, n: Int): Set[Set[Figure]] = {
    variants.toList.length === n
    asSet(variants)
  }

  def expectNReturn2(variants: Traversable[Figures], n: Int): Set[Set[Figure]] = {
    variants.toList.length === n
    asSet2(variants)
  }

  def asSet(variants: Flow) = variants.toSet[Figures].map(_.toSet)

  def asSet2(variants: Traversable[Figures]) = variants.toSet[Figures].map(_.toSet)
}
