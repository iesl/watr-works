package edu.umass.cs.iesl.watr
package spindex

class ConnectedComponentTest extends ConnectedComponentTestUtil {
  import ComponentOperations._

  behavior of "connected components"

  


  it should "create and flatten child structure" in {
    val mpageIndex = createMultiPageIndex(
      // 012 3 4567890
      """Eu1 - xBixVO4"""
    )

    for { row  <- labelRow(mpageIndex, 0, LB.VisualLine) } {

      // Create an ordering for page atoms
      row.connectChildren(LB.PageAtom, Some(_.bounds.left))

      // println(VisualLine.renderRoleTree(row))

      // Group all PageAtom children into one large sub group (also labeled TextSpan)
      row.groupChildren(withLabel=LB.PageAtom, newLabel=LB.TextSpan)({(atom1, atom2, pairIndex) =>
        true
      }, {(region, regionIndex) =>
        // Nothing to do here...
      })

      // println(VisualLine.renderRoleTree(row))

      row.ungroupChildren(LB.TextSpan)({atom1 =>
        true
      })
      // println(VisualLine.renderRoleTree(row))


    }
  }

  it should "demonstrate sup/subscript labeling" in {
    val mpageIndex = createMultiPageIndex(
      // 012 3 4567890
      """Eu1 - xBixVO4"""
    )

    //  when ccs are 'connected' they query is cached such that future lookups return the ordered children
    for { row  <- labelRow(mpageIndex, 0, LB.VisualLine) } {

      row.connectChildren(LB.PageAtom, Some(_.bounds.left))

      // Create a CC w/same target region, same atom sorting, linked as child to row
      val textSpanRegion = row.cloneAs(LB.TextSpan)

      row.setChildren(LB.TextSpan, Seq(textSpanRegion))
      row.addLabel(LB.Tokenized)

      // Region CCs r1,r2 become siblings in parent ordering tree, and inherit PageAtom ordering
      val splitRegions = textSpanRegion
        .splitAtomsIf((_, _, pairIndex) => Set(1, 4, 6, 7, 9).contains(pairIndex))

      splitRegions.zipWithIndex.foreach{case (r, i) =>
        if ((i % 2)==1) {
          r.addLabel(LB.Sub)
        }
        if (i==1) {
          val exprCCs = r.splitAtomsIf((_, _, pairIndex) => Set(0, 1).contains(pairIndex))
          r.setChildren(LB.TextSpan, exprCCs)
          r.addLabel(LB.Tokenized)
        }
      }

      textSpanRegion.setChildren(LB.TextSpan, splitRegions)

      // val textFlow = VisualLine.toTextReflow(row).get
      // val rendered = textFlow.toText()

      // println("Final Tree")
      // println(VisualLine.renderRoleTree(row))
      // println(s"rendered: ${rendered}")

      // assertResult("Eu_{1 - x}Bi_{x}VO_{4}"){
      //   rendered.toString()
      // }

      // ???

    }

  }


}
