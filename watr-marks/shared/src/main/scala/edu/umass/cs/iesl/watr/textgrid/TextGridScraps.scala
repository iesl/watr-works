package edu.umass.cs.iesl.watr
package textgrid


sealed trait Empty

/**

  def textGridToLabelingWidget(textGrid: TextGrid): Unit = {
    val labelTree = textGridToLabelTree(textGrid)

    val lls = flattenLabelTreeToLines(labelTree)

    val dbg = lls.map { _ match {

      case r0 @ LabeledRowElem.CellGroupRow(labels, cells) =>
        val text = cells.map(_.cells.map(_.char).mkString).mkString
        s"${labels}: ${text}"
      case r0 @ LabeledRowElem.HeadingRow(labels, heading) =>
        val text = heading
        s"${labels}: ${text}"
    }}

    println(
      dbg.mkString("{\n  ", "\n  ", "\n}")
    )


    // val gridTreeLocs = mapGridCellsToTreeLocs(textGrid)

    // val rows = gridTreeLocs.map { treeLocRow =>
    //   val rowText = treeLocRow.map(_.getLabel.cell.char).mkString
    //   val depth = treeLocRow.head.parents.length-1
    //   val headLoc = treeLocRow.head
    //   val headCell = headLoc.getLabel.cell
    //   val lastCell = treeLocRow.last.getLabel.cell

    //   // val mod1 = headLoc.pins.isEmpty ? -1 else tailLoc.pins.top.isLast || locRow.len==1 && headLoc.pins.top.isUnit
    //   // headCell.pins.isEmpty
    //   val rowHeaders = parentHeaders(headLoc).drop(1)
    //   val truncatedHeaders = rowHeaders.dropWhile { case (headerText, _) =>
    //     headerText == rowText
    //   }

    //   println(s"For Row ${rowText}")
    //   println("Row Headers")
    //   println(rowHeaders.map(si => s"${si._2}: ${si._1}").mkString("\n  ", "\n  ", "\n"))

    //   val headerList = truncatedHeaders.reverse

    //   println("Truncated Headers")
    //   println(truncatedHeaders.map(si => s"${si._2}: ${si._1}").mkString("\n  ", "\n  ", "\n"))

    //   println("Final Headers")
    //   println(headerList.map(si => s"${si._2}: ${si._1}").mkString("\n  ", "\n  ", "\n"))

    //   println()
    //   println()
    //   val indentedHdrs = headerList.map{ case (h, i) =>
    //     indent(i*Indent, "+".besideS(h))
    //   }

    //   if (headerList.nonEmpty) {
    //     vjoin(left,
    //       vjoins(left, indentedHdrs),
    //       indent((depth)*Indent, ">".besideS(rowText))
    //     )
    //   } else {
    //     indent((depth)*Indent, ">".besideS(rowText))
    //   }
    // }

    // val block = vjoins(left, rows)
    // println(block.toString)

    // - output is a list of grid data points, with bboxes, classes,  or spacers, which may also have classes
    //     or create indentation. classes allow hover highlighting for indentation spaces
    // - to create left-side controls...
    //   - "Marginalize" the BIO labels
  }

  // def getTextAtTreeLoc(loc: TreeLoc[TreeNode]): String = {
  //   val rleaves = loc.tree.loc.cojoin.toStream.filter(_.isLeaf)
  //   rleaves.map(_.getLabel.asInstanceOf[TreeNode.CellNode].cell.char).mkString
  // }

  // def parentHeaders(loc: TreeLoc[TreeNode.CellNode]): Seq[(String, Int)] = {
  //   def _parentHeaders(loc: TreeLoc[TreeNode]): Seq[(String, Int)] = {
  //     val parentNotRoot = loc.parents.length > 1
  //     if (loc.isFirst && parentNotRoot) {
  //       loc.parent.map{ p =>
  //         (getTextAtTreeLoc(p), p.parents.length-1) +: _parentHeaders(p)
  //       } getOrElse {
  //         Seq()
  //       }
  //     } else {
  //       Seq()
  //     }
  //   }
  //   _parentHeaders(loc.asInstanceOf[TreeLoc[TreeNode]])
  // }

  // def mapGridCellsToTreeLocs(textGrid: TextGrid): Seq[Seq[TreeLoc[TreeNode.CellNode]]] = {
  //   val labelTree = textGridToLabelTree(textGrid)

  //   val leafLocs: List[TreeLoc[TreeNode.CellNode]] = labelTree.loc.cojoin.toStream
  //     .filter(_.isLeaf)
  //     .map(_.asInstanceOf[TreeLoc[TreeNode.CellNode]])
  //     .toList

  //   val init = List[List[TreeLoc[TreeNode.CellNode]]]()

  //   val rowLengths = textGrid.rows.map(_.cells.length)
  //   val locRowsR = rowLengths.foldLeft { (init, leafLocs) } {
  //     case ((acc, locs), e) =>
  //       val (row, rest) = (locs.take(e), locs.drop(e))
  //       val asdf: List[List[TreeLoc[TreeNode.CellNode]]] = row +: acc
  //       // val asdf: List[List[TreeLoc[TreeNode.CellNode]]] = acc

  //       (asdf, rest)
  //   }

  //   locRowsR._1.reverse

  // }

  */
