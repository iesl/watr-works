package org.watrworks
package utils


trait AppMainBasics {

  def argsToMap(args: Seq[String]): Map[String, List[String]] = {
    import scala.collection.mutable.{ListMap => LMap}
    // import scala.collection.immutable.ListMap

    val argmap = LMap[String, List[String]]()
    args.foldLeft(argmap)({(m, k:String) => {
      val ss:Seq[Char] = k
      ss match {
        case Seq('-', '-', opt @ _*) => m.put(opt.toString, List[String]())
        case Seq('-', opt @ _*) => m.put(opt.toString, List[String]())
        case opt @ _ => m.put(m.head._1, m.head._2 ++ List[String](opt.toString))
      }
      m
    }})
    Map[String, List[String]](argmap.toList.reverse: _*)
  }

}
