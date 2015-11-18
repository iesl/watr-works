package edu.umass.cs.iesl.watr
package nashorn

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, ScheduledExecutorService, TimeUnit, ScheduledFuture}
import java.util.function.Function
import jdk.nashorn.api.scripting.JSObject

import scala.annotation.tailrec

class WindowTimers(executorService: ScheduledExecutorService) {

  private val sequenceNumber = new AtomicInteger(0)
  private val scheduledFutures = new ConcurrentHashMap[Integer, (Integer, ScheduledFuture[_])]

  def setTimeout(handler: JSObject, delayMs: Long, args: AnyRef*): AnyRef = {
    require(handler.isFunction, "setTimeout handler must be a function")
    val scheduledFuture = executorService.schedule(new Runnable {
      override def run(): Unit = handler.call(null, args: _*)
    }, delayMs, TimeUnit.MILLISECONDS)

    saveScheduledFuture(scheduledFuture)
  }

  def setInterval(handler: JSObject, delayMs: Long, intervalMs: Long, args: AnyRef*): AnyRef = {
    require(handler.isFunction, "setInterval handler must be a function")

    // I'm not 100% sure, but it seems as if we should use fixed delay rather than fixed rate:
    // https://html.spec.whatwg.org/multipage/webappapis.html#timer-initialisation-steps
    val scheduledFuture = executorService.scheduleWithFixedDelay(new Runnable {
      override def run(): Unit = handler.call(null, args: _*)
    }, delayMs, intervalMs, TimeUnit.MILLISECONDS)

    saveScheduledFuture(scheduledFuture)
  }


  def clearTimeout(id: AnyRef): Unit = cancelScheduledFuture(id)

  def clearInterval(id: AnyRef): Unit = cancelScheduledFuture(id)

  private def cancelScheduledFuture(id: AnyRef): Unit = id match {
    case numericalId: Integer =>
      Option(scheduledFutures.remove(numericalId)) foreach { sf => sf._2.cancel(false) }
    case _ => // noop
  }

  @tailrec
  private def saveScheduledFuture(sf: ScheduledFuture[_]): Integer = {
    val key: Integer = sequenceNumber.incrementAndGet()

    // This was way harder than I expected...
    val fun: Function[Integer, (Integer, ScheduledFuture[_])] = new Function[Integer, (Integer, ScheduledFuture[_])] {
      override def apply(t: Integer): (Integer, ScheduledFuture[_]) = (t, sf)
    }
    val fun2: Function[_ >: Integer, _ <: (Integer, ScheduledFuture[_])] = fun
    val result = scheduledFutures.computeIfAbsent(key, fun2)

    // If the key in the result matches the key we tried to put, then we're done.
    if (result._1 == key) return key

    // Failed to put the expected map entry, so try again.
    saveScheduledFuture(sf)
  }
}
