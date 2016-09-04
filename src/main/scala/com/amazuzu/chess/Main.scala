package com.amazuzu.chess

import com.amazuzu.chess.services.ChessBoard

/**
  * Created by taras on 9/3/16.
  */
object Main extends App {

  if (args.length != 2) usage("wrong arguments count")
  else {
    val start = System.currentTimeMillis()

    if (
      """\d+x\d+""".r.pattern.matcher(args(0)).matches()) {
      val Array(m, n) = args(0).split("x").map(_.toInt)

      if (
        """[NQBRK]+""".r.pattern.matcher(args(1)).matches()) {

        val figures: Row = args(1).toList

        //val list = ChessBoard(m, n, figures).variants
        val list = ChessBoard(m, n, figures).variantsView


        var counter: Long = 0L
        list.foreach { el =>
          counter += 1
        }

        println(counter + " variants in " + Math.round((System.currentTimeMillis() - start) / 1000.0) + " seconds")

      } else usage(s"wrong figures set '${args(1)}'")


    } else usage(s"wrong board dimension '${args(0)}'")

  }

  def usage(msg: String) = println(s"""usage: sbt "run 7x7 NNQQBBK"\nerror: $msg """)

}
