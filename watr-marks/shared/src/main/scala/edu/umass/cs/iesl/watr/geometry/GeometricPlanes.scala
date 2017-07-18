package edu.umass.cs.iesl.watr
package geometry

// import utils.ExactFloats._
// import utils.{CompassDirection => CDir}

object GeometricPlanes {


  class EnclosedFigure(fig: LTBounds, plane: LTBounds) {

  }

  // def splitVertical(x: Int@@FloatRep): Option[(LTBounds, LTBounds)] = {
  //   if (overlapsX(x)) {
  //     val LTBounds(left, top, width, height) = theBbox
  //     val bleft = LTBounds(left, top, x-left, height)
  //     val bright = LTBounds(x, top, width-bleft.width, height)

  //     Some((bleft, bright))
  //   } else None
  // }

  // def splitHorizontal(y: Int@@FloatRep): Option[(LTBounds, LTBounds)] = {
  //   if (overlapsY(y)) {
  //     val LTBounds(left, top, width, height) = theBbox
  //     val upper = LTBounds(left, top, width, y-top)
  //     val lower = LTBounds(left, y, width, height-upper.height)

  //     Some((upper, lower))
  //   } else None
  // }



    // /* TODO this rectangle split function is useful but has bugs.
    //  Split this rectangle into 0-3 parts, depending on overlap, like so:
    //                    +-------------+
    //                    |             |  <- splitter bbox
    //    +---------------+-------------+-----------------+
    //    |               |             |                 |    <-- self bbox
    //    |               |             |                 |
    //    |     left      |     mid     |      right      |
    //    |     bbox      |    bbox     |       bbox      |
    //    |               |             |                 |
    //    +---------------+-------------+-----------------+
    //                    |             |
    //                    |             |
    //                    +-------------+

    //  */
    // def splitHorizontal(splitter: LTBounds): List[LTBounds] = {
    //   val leftX = splitter.toPoint(CDir.W).x
    //   val rightX = splitter.toPoint(CDir.E).x
    //   val self = theBbox
    //   val leftRights = if (self.intersectsX(leftX)){
    //     val splitLeft = self.splitHorizontal(leftX)
    //     if (self.intersectsX(rightX)){
    //       val splitRight = self.splitHorizontal(rightX)
    //       splitLeft.head :: splitLeft.tail
    //     } else {
    //       List(splitLeft.head)
    //     }
    //   } else if (self.intersectsX(rightX)){
    //     self.splitHorizontal(rightX).tail
    //   } else {
    //     List()
    //   }

    //   leftRights
    // }
    // import scalaz.{@@ => _, _} , Scalaz._
    // import utils.FunctionalHelpers.IO

    // def adjacentRegionWithin(enclosingRegion: LTBounds, cDir: CDir): Option[LTBounds] = {
    //   println(s"(${theBbox}).adjacentRegionWithin($enclosingRegion)   ${cDir}  ")
    //   cDir match {
    //     case CDir.N  =>
    //       for {
    //         (l, right) <- enclosingRegion.splitVertical(theBbox.left)
    //         // _          <- IO.putStrLn[Option](s"  (l, right) = ($l, $right)")
    //         (left, r)  <- right.splitVertical(theBbox.right)
    //         // _          <- IO.putStrLn[Option](s"  (left, r) = ($left, $r)")
    //         (top, b)   <- left.splitHorizontal(theBbox.top)
    //         // _          <- IO.putStrLn[Option](s"  (top, b) = ($top, $b)")
    //       } yield top

    //     case CDir.S  =>
    //       for {
    //         (l, right) <- enclosingRegion.splitVertical(theBbox.left)
    //         (left, r)  <- right.splitVertical(theBbox.right)
    //         (t, bot)   <- left.splitHorizontal(theBbox.bottom)
    //       } yield bot


    //     case CDir.E  =>
    //       for {
    //         (t, bot)   <- enclosingRegion.splitHorizontal(theBbox.top)
    //         // _          <- IO.putStrLn[Option](s"  (t, bot) = ($t, $bot)")
    //         (top, b)   <- bot.splitHorizontal(theBbox.bottom)
    //         // _          <- IO.putStrLn[Option](s"  (top, b) = ($top, $b)")
    //         (l, right) <- top.splitVertical(theBbox.right)
    //         // _          <- IO.putStrLn[Option](s"  (l, right) = ($l, $right)")
    //       } yield right

    //     case CDir.W  =>
    //       for {
    //         (t, bot)  <- enclosingRegion.splitHorizontal(theBbox.top)
    //         // _         <- IO.putStrLn[Option](s"  (t, bot) = ($t, $bot)")
    //         (top, b)  <- bot.splitHorizontal(theBbox.bottom)
    //         // _         <- IO.putStrLn[Option](s"  (top, b) = ($top, $b)")
    //         (left, r) <- top.splitVertical(theBbox.left)
    //         // _         <- IO.putStrLn[Option](s"  (left, r) = ($left, $r)")
    //       } yield left

    //     case CDir.NW => ???
    //     case CDir.SW => ???
    //     case CDir.NE => ???
    //     case CDir.SE => ???
    //   }
    // }
}
