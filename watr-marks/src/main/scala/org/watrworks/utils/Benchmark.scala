package org.watrworks
package utils

object Timer {

  def time[R](prefix: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println(s"${prefix}: " + (t1 - t0) / 1000000.0d + "ms")
    result
  }

}
