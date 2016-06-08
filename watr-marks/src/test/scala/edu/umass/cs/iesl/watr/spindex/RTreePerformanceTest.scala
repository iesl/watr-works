package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

class RTreePerformanceTest extends FlatSpec {

  import gnu.trove.procedure.TIntProcedure

  import net.sf.jsi.SpatialIndex
  import net.sf.jsi.rtree.RTree
  import net.sf.jsi.Rectangle

  behavior of "timing stats"

  it should  "create index" in {

    // val version = libspatialindex.SIDX_Version()  // Create and initialize an rtree
    val si: SpatialIndex = new RTree()
    si.init(null)

    {
      // These timings are from libspatial index (c-lib w/java bindings) for comparison
      //   time to insert 1000= 259 ms
      //   time to insert 10000= 1621 ms
      //   time to insert 50000= 5507 ms

      // These timings are from tests below
      //   time to insert 1000= 18 ms
      //   time to insert 10000= 72 ms
      //   time to insert 100000= 276 ms


      val iterations = 1000
      var i = 2
      val start = System.currentTimeMillis()
      while (i < iterations) {
        val fi = i.toFloat
        val fi2 = ((i+1)*2).toFloat
        val r = new Rectangle(fi, fi, fi2, fi2)
        si.add(r, i)
        i += 1
      }

      val end = System.currentTimeMillis()
      println(s"time to insert ${iterations}= ${(end-start)} ms")
    }

    case class Stats(iter: Int, time: Long, nres: Int)

    class SaveToListProcedure extends TIntProcedure {
      // import scala.collection.mutable
      // val ids = mutable.ArrayBuffer[Int]()
      var n: Int = 0


      override def execute(id: Int): Boolean = {
        // ids += id
        n += 1
        true
      }

      def idCount = n // ids.length
    }


    val timings:List[Stats] = (
      for(i <- 2 to 20) yield {
        val myProc = new SaveToListProcedure()

        val start = System.currentTimeMillis()
        val fi = (i-1).toFloat
        val fi2 = ((i+2)*2).toFloat
        val r = new Rectangle(fi, fi, fi2, fi2)
        si.intersects(r, myProc)

        val end = System.currentTimeMillis()

        Stats(
          i,
          end - start,
          myProc.idCount
        )
      }) toList

    println(timings.mkString("\n"))
  }

}
