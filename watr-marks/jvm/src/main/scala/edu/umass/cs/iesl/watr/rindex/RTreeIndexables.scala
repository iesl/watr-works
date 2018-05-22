package edu.umass.cs.iesl.watr
package rtrees

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import edu.umass.cs.iesl.watr.{geometry => G}
import com.github.davidmoten.rtree.{geometry => RG}

// trait RTreeIndexable[+T] {
//   def id[C <: T](t: C): Int
//   def ltBounds[C <: T](t: C): G.LTBounds
//   def rtreeGeometry[C <: T](t: C): RG.Geometry
// }
