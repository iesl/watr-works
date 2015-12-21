package edu.umass.cs.iesl.watr.watrmarks

import dom._

case class BioCursor(
  focii: Seq[(DomCursor, BrickCursor)]
) {

  def getText: String =  {
    focii.map({ case (dcur, bcur) =>
      bcur.current.map(_.char).mkString
    }).mkString
  }

  def next: Option[BioCursor] = {
    focii.reduce({(cur0,cur1) =>
      val (dcur0, bcur0) = cur0
      val (dcur1, bcur1) = cur1


      (dcur0, bcur1)
    })


    val (dcur, bcur) = focii.last

    bcur.next match {
      case Some(nbcur) =>
      case None =>
    }
    None
  }


  def foreach(f: (BioCursor) => Unit): Unit = {
    println("running foreach ")

    f(this)
    next match {
      case Some(ncur) => ncur.foreach(f)
      case None =>
    }
  }

  override def toString = {
    val fs = focii.map{ case (dcur,bcur) =>

      println(s"dcur: ${dcur}")
      println(s"bcur: ${bcur}")

      s""" ${dcur.toString}
      ${bcur.toString}"""
    }.mkString("\n")

    s"cur<${fs}>"
  }

}
