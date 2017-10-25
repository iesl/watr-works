package edu.umass.cs.iesl.watr
package rindex

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import edu.umass.cs.iesl.watr.{geometry => G}
import com.github.davidmoten.rtree.{geometry => RG}

trait RTreeIndexable[T] {
  def id(t: T): Int
  def ltBounds(t: T): G.LTBounds
  def rtreeGeometry(t: T): RG.Geometry

  def serialize[C <: T](value: C): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close
    stream.toByteArray
  }

  def deserialize[C <: T](bytes: Array[Byte]): C = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close
    value.asInstanceOf[C]
  }
}
