package edu.umass.cs.iesl.watr
package utils

import Histogram._

object Histogram {

  private val EPSILON = 1.0e-6

  def fromValues(samples: Seq[Double], resolution: Double): Histogram = {
    var min = java.lang.Double.POSITIVE_INFINITY
    var max = java.lang.Double.NEGATIVE_INFINITY
    for (sample <- samples) {
      min = Math.min(min, sample)
      max = Math.max(max, sample)
    }
    val histogram = new Histogram(min, max, resolution)
    for (sample <- samples) {
      histogram.add(sample)
    }
    histogram
  }

  def histogram(min: Double, max: Double, resolution: Double): Histogram = {
    new Histogram(min, max, resolution)
  }

  def histogram(values: Seq[Double], resolution: Double): Histogram = {
    Histogram.fromValues(values, resolution)
  }

  def getMostFrequentValues(in: Seq[Double], resolution: Double): Seq[(Double, Double)] = {
    histogram(in, resolution)
      .getFrequencies
      .sortBy(_.frequency)
      .reverse
      .takeWhile(_.frequency > 0)
      .map{b=>(b.value, b.frequency)}
  }
}

class Histogram(minValue: Double, maxValue: Double, _resolution: Double) {

  class Bin (var index: Int) {
    val frequency: Double = frequencies(index)
    val value: Double = (index + 0.5) * resolution + min
  }


  private val min: Double = minValue - EPSILON

  private val delta: Double = maxValue - minValue + 2 * EPSILON

  val size = Math.max(1, Math.round((maxValue - minValue) / _resolution).toInt)

  private val resolution: Double = delta / size

  private var frequencies: Array[Double] = Array.fill(size)(0)

  def smooth(windowLength: Double): Unit = {
    val size = Math.round(windowLength / resolution).toInt / 2
    var sum = 0.0
    var i = 0
    while (i <= size) {
      sum += frequencies(i)
      i += 1
    }
    val newFrequencies = Array.ofDim[Double](frequencies.length)
    for (i <- 0 until size) {
      newFrequencies(i) = sum / (2 * size + 1)
      sum += frequencies(i + size + 1)
    }
    for (i <- size until frequencies.length - size - 1) {
      newFrequencies(i) = sum / (2 * size + 1)
      sum += frequencies(i + size + 1)
      sum -= frequencies(i - size)
    }
    for (i <- frequencies.length - size - 1 until frequencies.length) {
      newFrequencies(i) = sum / (2 * size + 1)
      sum -= frequencies(i - size)
    }
    frequencies = newFrequencies
  }

  def circularSmooth(windowLength: Double): Unit = {
    val size = Math.round(windowLength / resolution).toInt / 2
    var sum = frequencies(0)
    var i = 1
    while (i <= size) {
      sum += frequencies(i) + frequencies(frequencies.length - i)
      i += 1
    }
    val newFrequencies = Array.ofDim[Double](frequencies.length)
    for (i <- 0 until frequencies.length) {
      newFrequencies(i) = sum / (2 * size + 1)
      sum += frequencies(if (i + size + 1 < frequencies.length) i + size + 1 else i + size + 1 - frequencies.length)
      sum -= frequencies(if (i - size < 0) frequencies.length + i - size else i - size)
    }
    frequencies = newFrequencies
  }

  def kernelSmooth(kernel: Array[Double]): Unit = {
    val newFrequencies = Array.ofDim[Double](frequencies.length)
    val shift = (kernel.length - 1) / 2
    for (i <- 0 until kernel.length) {
      val jStart = Math.max(0, i - shift)
      val jEnd = Math.min(frequencies.length, frequencies.length + i - shift)
      for (j <- jStart until jEnd) {
        newFrequencies(j - i + shift) += kernel(i) * frequencies(j)
      }
    }
    frequencies = newFrequencies
  }

  def circularKernelSmooth(kernel: Array[Double]): Unit = {
    val newFrequencies = Array.ofDim[Double](frequencies.length)
    val shift = (kernel.length - 1) / 2
    for (i <- 0 until frequencies.length; d <- 0 until kernel.length) {
      var j = i + d - shift
      if (j < 0) {
        j += frequencies.length
      } else if (j >= frequencies.length) {
        j -= frequencies.length
      }
      newFrequencies(i) += kernel(d) * frequencies(j)
    }
    frequencies = newFrequencies
  }


  def createGaussianKernel(length: Double, _stdDeviation: Double): Array[Double] = {
    val r = Math.round(length / resolution).toInt / 2
    val stdDeviation  = _stdDeviation / resolution
    val size = 2 * r + 1
    val kernel = Array.ofDim[Double](size)
    var sum: Double = 0
    val b = 2 * stdDeviation * stdDeviation
    val a = 1 / Math.sqrt(Math.PI * b)
    for (i <- 0 until size) {
      kernel(i) = a * Math.exp(-(i - r) * (i - r) / b)
      sum += kernel(i)
    }
    for (i <- 0 until size) {
      kernel(i) /= sum
    }
    kernel
  }

  def circularGaussianSmooth(windowLength: Double, stdDeviation: Double): Unit = {
    circularKernelSmooth(createGaussianKernel(windowLength, stdDeviation))
  }

  def gaussianSmooth(windowLength: Double, stdDeviation: Double): Unit = {
    kernelSmooth(createGaussianKernel(windowLength, stdDeviation))
  }

  def add(value: Double): Unit = {
    frequencies(((value - min) / resolution).toInt) += 1.0
  }

  def getSize(): Int = frequencies.length

  def getFrequency(index: Int): Double = frequencies(index)

  def getPeakValue(): Double = {
    var peakIndex = 0
    for (i <- 1 until frequencies.length if frequencies(i) > frequencies(peakIndex)) {
      peakIndex = i
    }
    var peakEndIndex = peakIndex + 1
    val EPS = 0.0001
    while (peakEndIndex < frequencies.length &&
      Math.abs(frequencies(peakEndIndex) - frequencies(peakIndex)) < EPS) {
      peakEndIndex += 1
    }
    (peakIndex.toDouble + peakEndIndex) / 2 * resolution + min
  }

  def getFrequencies(): Seq[Bin] = (0 until frequencies.length).map{ new Bin(_) }

}
