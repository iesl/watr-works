package edu.umass.cs.iesl.watr
package utils

import java.util.concurrent._


/**
  * Full text at  https://gist.github.com/adamchandra/38a0f6b1d3b96ebf67971a121a342c7c.js
  *
  * Thread pools on the JVM should usually be divided into the following three categories:
  *
  *    1. CPU-bound
  *    2. Blocking IO
  *    3. Non-blocking IO polling
  *
  */
object Threading {

  import java.util.concurrent.Executors
  import scala.concurrent._

  val MainCPUBound = ExecutionContext.global
  val BlockingFileIO = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val NonBlockingIOPolling = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))


  implicit val DefaultThreadPool = MainCPUBound

}


//DEBUG USE ONLY!
class ThreadPoolExecutorMonitor(executor: ThreadPoolExecutor, delaySeconds: Int = 1) extends Runnable {
  // Taken from Code from 'Don't Cheat the Executor' nescala talk
  private var running = true
  def finish(): Unit = {
    running = false
  }

  def run(): Unit = {
    while (running) {
      println(s"""
                  |Thread Pool Stats
                  |Pool Size:${executor.getPoolSize}
                  |Core Pool Size: ${executor.getCorePoolSize}
                  |Active Count: ${executor.getActiveCount}
                  |Completed Count: ${executor.getCompletedTaskCount}
                  |Current Queue Size: ${executor.getQueue.size}""".stripMargin)
      Thread.sleep(1000L * delaySeconds)
    }
  }

}

//DEBUG USE ONLY!
class ForkJoinMonitor(forkJoinPool: ForkJoinPool, delaySeconds: Int = 1) extends Runnable {

  private var running = true
  def finish(): Unit = {
    running = false
  }

  def run(): Unit = {
    while (running) {
      println(s"""
                  |Pool Size:${forkJoinPool.getPoolSize}
                  |Active Thread Count: ${forkJoinPool.getActiveThreadCount}
                  |Running Thread Count: ${forkJoinPool.getActiveThreadCount}
                  |Queued Submissions: ${forkJoinPool.getQueuedSubmissionCount}
                  |Queued Tasks: ${forkJoinPool.getQueuedTaskCount}
                  |Has Queued Submissions: ${forkJoinPool.hasQueuedSubmissions}
                  |""".stripMargin)
      Thread.sleep(1000L * delaySeconds)
    }
  }

}
