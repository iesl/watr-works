package edu.umass.cs.iesl.watr
package utils

import ExactFloats._

object Histogram {

  // private val EPSILON = 1.0e-6
  private val EPSILON = 0.01.toFloatExact()

  def fromValues(samples: Seq[FloatExact], resolution: Double): Histogram = {
    val min = samples.min
    val max = samples.max
    val histogram = new Histogram(min, max, resolution)
    for (sample <- samples) {
      histogram.add(sample)
    }
    histogram
  }

  def histogram(min: FloatExact, max: FloatExact, resolution: Double): Histogram = {
    new Histogram(min, max, resolution)
  }

  def histogram(values: Seq[FloatExact], resolution: Double): Histogram = {
    Histogram.fromValues(values, resolution)
  }



}
import Histogram._
import scala.collection.mutable

class Histogram(minValue: FloatExact, maxValue: FloatExact, _resolution: Double) {

  class Bin (index: Int) {
    val frequency: Double = frequencies(index)
    val value: FloatExact = (index.toFloatExact() + 0.5) * resolution + min
  }

  private val min: FloatExact = minValue - EPSILON

  private val delta: FloatExact = maxValue - minValue + 2.toFloatExact * EPSILON

  val size = math.max(1, math.round((maxValue - minValue).asDouble() / _resolution).toInt+1)

  private val resolution: Double = (delta / size).asDouble

  val frequencies: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer.fill(size)(0d)
  println(s"init'd frequencies to ${frequencies}, ${frequencies.length}: min:${min}, delta:${delta} size:${size}")

  def add(value: FloatExact): Unit = {
    val bin = ((value - min) / resolution)
    println(s"adding ${value.pp} w/bin ${bin}, asInt:${bin.asInt()}")
    frequencies(bin.asInt) += 1.0
  }

  def getSize(): Int = frequencies.length

  def getFrequency(index: Int): Double = frequencies(index)

  def getFrequencies(): Seq[Bin] = (0 until frequencies.length).map{ new Bin(_) }

  def getStartingResolution(): Double = _resolution
  def getComputedResolution(): Double = resolution



  // def smooth(windowLength: Double): Histogram = {
  //   val size = Math.round(windowLength / resolution).toInt / 2
  //   var sum = 0.0
  //   var i = 0
  //   while (i <= size) {
  //     sum += frequencies(i)
  //     i += 1
  //   }
  //   val newFrequencies = Array.ofDim[Double](frequencies.length)
  //   for (i <- 0 until size) {
  //     newFrequencies(i) = sum / (2 * size + 1)
  //     sum += frequencies(i + size + 1)
  //   }
  //   for (i <- size until frequencies.length - size - 1) {
  //     newFrequencies(i) = sum / (2 * size + 1)
  //     sum += frequencies(i + size + 1)
  //     sum -= frequencies(i - size)
  //   }
  //   for (i <- frequencies.length - size - 1 until frequencies.length) {
  //     newFrequencies(i) = sum / (2 * size + 1)
  //     sum -= frequencies(i - size)
  //   }
  //   frequencies = newFrequencies

  //   this
  // }

  // def circularSmooth(windowLength: Double): Unit = {
  //   val size = Math.round(windowLength / resolution).toInt / 2
  //   var sum = frequencies(0)
  //   var i = 1
  //   while (i <= size) {
  //     sum += frequencies(i) + frequencies(frequencies.length - i)
  //     i += 1
  //   }
  //   val newFrequencies = Array.ofDim[Double](frequencies.length)
  //   for (i <- 0 until frequencies.length) {
  //     newFrequencies(i) = sum / (2 * size + 1)
  //     sum += frequencies(if (i + size + 1 < frequencies.length) i + size + 1 else i + size + 1 - frequencies.length)
  //     sum -= frequencies(if (i - size < 0) frequencies.length + i - size else i - size)
  //   }
  //   frequencies = newFrequencies
  // }

  // def kernelSmooth(kernel: Array[Double]): Unit = {
  //   val newFrequencies = Array.ofDim[Double](frequencies.length)
  //   val shift = (kernel.length - 1) / 2
  //   for (i <- 0 until kernel.length) {
  //     val jStart = Math.max(0, i - shift)
  //     val jEnd = Math.min(frequencies.length, frequencies.length + i - shift)
  //     for (j <- jStart until jEnd) {
  //       newFrequencies(j - i + shift) += kernel(i) * frequencies(j)
  //     }
  //   }
  //   frequencies = newFrequencies
  // }

  // def circularKernelSmooth(kernel: Array[Double]): Unit = {
  //   val newFrequencies = Array.ofDim[Double](frequencies.length)
  //   val shift = (kernel.length - 1) / 2
  //   for (i <- 0 until frequencies.length; d <- 0 until kernel.length) {
  //     var j = i + d - shift
  //     if (j < 0) {
  //       j += frequencies.length
  //     } else if (j >= frequencies.length) {
  //       j -= frequencies.length
  //     }
  //     newFrequencies(i) += kernel(d) * frequencies(j)
  //   }
  //   frequencies = newFrequencies
  // }


  // def createGaussianKernel(length: Double, _stdDeviation: Double): Array[Double] = {
  //   val r = Math.round(length / resolution).toInt / 2
  //   val stdDeviation  = _stdDeviation / resolution
  //   val size = 2 * r + 1
  //   val kernel = Array.ofDim[Double](size)
  //   var sum: Double = 0
  //   val b = 2 * stdDeviation * stdDeviation
  //   val a = 1 / Math.sqrt(Math.PI * b)
  //   for (i <- 0 until size) {
  //     kernel(i) = a * Math.exp(-(i - r) * (i - r) / b)
  //     sum += kernel(i)
  //   }
  //   for (i <- 0 until size) {
  //     kernel(i) /= sum
  //   }
  //   kernel
  // }

  // def circularGaussianSmooth(windowLength: Double, stdDeviation: Double): Unit = {
  //   circularKernelSmooth(createGaussianKernel(windowLength, stdDeviation))
  // }

  // def gaussianSmooth(windowLength: Double, stdDeviation: Double): Unit = {
  //   kernelSmooth(createGaussianKernel(windowLength, stdDeviation))
  // }


  // def getPeakValue(): FloatExact = {
  //   var peakIndex = 0
  //   for (i <- 1 until frequencies.length if frequencies(i) > frequencies(peakIndex)) {
  //     peakIndex = i
  //   }
  //   var peakEndIndex = peakIndex + 1
  //   val EPS = 0.0001
  //   while (peakEndIndex < frequencies.length &&
  //     Math.abs(frequencies(peakEndIndex) - frequencies(peakIndex)) < EPS) {
  //     peakEndIndex += 1
  //   }
  //   (peakIndex.toDouble + peakEndIndex) / 2 * resolution + min

  //   // (peakIndex.toDouble + peakEndIndex) / 2 * resolution + min
  // }


}
