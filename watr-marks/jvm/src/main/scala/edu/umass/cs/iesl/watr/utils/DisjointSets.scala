package edu.umass.cs.iesl.watr
package utils 

import java.util._
import DisjointSets._
import scala.collection.JavaConversions._

object DisjointSets {

  def singletonsOf[E <: Enum[E]](elementType: Class[E]): DisjointSets[E] = {
    new DisjointSets(Arrays.asList(elementType.getEnumConstants:_*))
  }

  class Entry[E](private var value: E) {

    var size: Int = 1

    var parent: Entry[E] = this

    var next: Entry[E] = null

    var last: Entry[E] = this

    def mergeWith(otherRepresentative: Entry[E]): Unit = {
      size += otherRepresentative.size
      last.next = otherRepresentative
      last = otherRepresentative.last
      otherRepresentative.parent = this
    }

    def findRepresentative(): Entry[E] = {
      var representative = parent
      while (representative.parent != representative) {
        representative = representative.parent
      }
      var entry = this
      while (entry != representative) {
        val nextEntry = entry.parent
        entry.parent = representative
        entry = nextEntry
      }
      representative
    }

    def isRepresentative(): Boolean = parent == this

    def asSet(): Set[E] = {
      new AbstractSet[E]() {

        override def iterator(): Iterator[E] = {
          new Iterator[E]() {

            private var nextEntry: Entry[E] = findRepresentative()

            override def hasNext(): Boolean = nextEntry != null

            override def next(): E = {
              if (nextEntry == null) {
                throw new NoSuchElementException()
              }
              val result = nextEntry.value
              nextEntry = nextEntry.next
              result
            }

            override def remove(): Unit = {
              throw new UnsupportedOperationException()
            }
          }
        }

        override def size(): Int = findRepresentative().size
      }
    }
  }
}

class DisjointSets[E](c: Collection[_ <: E]) extends java.lang.Iterable[Set[E]] {

  private var map: Map[E, Entry[E]] = new HashMap[E, Entry[E]]()

  for (element <- c) {
    map.put(element, new Entry[E](element))
  }

  def areTogether(e1: E, e2: E): Boolean = {
    map.get(e1).findRepresentative() == map.get(e2).findRepresentative()
  }

  def union(e1: E, e2: E): Unit = {
    val r1 = map.get(e1).findRepresentative()
    val r2 = map.get(e2).findRepresentative()
    if (r1 != r2) {
      if (r1.size <= r2.size) {
        r2.mergeWith(r1)
      } else {
        r1.mergeWith(r2)
      }
    }
  }

  override def iterator(): Iterator[Set[E]] = {
    new Iterator[Set[E]]() {

      private var iterator: Iterator[Entry[E]] = map.values.iterator()

      private var nextRepresentative: Entry[E] = _

      findNextRepresentative()

      override def hasNext(): Boolean = nextRepresentative != null

      override def next(): Set[E] = {
        if (nextRepresentative == null) {
          throw new NoSuchElementException()
        }
        val result = nextRepresentative.asSet()
        findNextRepresentative()
        result
      }

      private def findNextRepresentative(): Unit = {
        while (iterator.hasNext) {
          val candidate = iterator.next()
          if (candidate.isRepresentative) {
            nextRepresentative = candidate
            return
          }
        }
        nextRepresentative = null
      }

      override def remove(): Unit = {
        throw new UnsupportedOperationException()
      }
    }
  }
}
