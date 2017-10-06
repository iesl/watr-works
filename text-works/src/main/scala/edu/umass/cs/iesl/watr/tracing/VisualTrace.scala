package edu.umass.cs.iesl.watr
package tracing

import tracemacros._
import scala.language.experimental.macros
import geometry._
import scala.collection.mutable
import VisualTraceLevel._

// import textboxing.{TextBoxing => TB}

import play.api.libs.json, json._

case class LogSpec(
  logname: String
)

object VisualTracer {

  { // Set trace levels here to force recompile and macro re-eval
    VisualTraceGlobals.addTraceLevel(VisualTraceLevel.JsonLogs)

  }


  def tracingEnabled(): Boolean = {
    VisualTraceGlobals.tracingEnabled()
  }

  type PageLogBuffer = mutable.HashMap[
    String,
    mutable.ArrayBuffer[
      mutable.ArrayBuffer[JsObject]
    ]
  ]

  def PageLogBuffer() = mutable.HashMap[
    String,
    mutable.ArrayBuffer[
      mutable.ArrayBuffer[JsObject]
    ]
  ]()


  // lazy val pageLogs = mutable.ArrayBuffer[PageLogBuffer]()
  // lazy val pageLogs = mutable.HashMap[Int@@PageNum, PageLogBuffer]()

  // def clearPages(): Unit = {
  //   pageLogs.clear()
  // }

  // def newPage(): Unit = {
  //   pageLogs += PageLogBuffer()
  // }

  // def currentLog(): PageLogBuffer = {
  //   pageLogs.last
  // }

  // def createLog(logname: String): Unit = {
  //   assume(!currentLog.contains(logname))
  //   val logsBuffer = currentLog.getOrElseUpdate(logname,
  //     mutable.ArrayBuffer()
  //   )
  //   logsBuffer += mutable.ArrayBuffer()
  // }

  // def appendLog(pageNum: Int@@PageNum, logname: String, log: json.JsObject): Unit = {
  //   val pageLog = pageLogs.getOrElseUpdate(pageNum, PageLogBuffer())

  //   val namedLogs = pageLog.getOrElseUpdate(logname,
  //     mutable.ArrayBuffer()
  //   )

  //   namedLogs.get(logname).foreach { logBuffers =>
  //     val currLogs = logBuffers.last
  //     currLogs += log
  //   }
  // }

  // def emitLogs(): JsValue = {
  //   val allLogs = for {
  //     (pageBuffer, page) <- pageLogs.zipWithIndex
  //     (logname, logInstances) <- pageBuffer
  //     logInstance <- logInstances
  //   } yield {
  //     // ("name", logname),
  //     Json.obj(
  //       ("name", s"${logname}-page-${page+1}"),
  //       ("steps", logInstance)
  //     )

  //   }

  //   Json.toJson(allLogs)
  // }
}

protected [tracing] object VisualTraceGlobals {

  val activeTraces = mutable.HashSet[VisualTraceLevel]()

  def tracingEnabled(): Boolean = {
    activeTraces.nonEmpty
  }

  def clearTraceLevels(): Unit = {
    activeTraces.clear()
  }

  def addTraceLevel(v: VisualTraceLevel): Unit = v match {
    case  EnterExit =>
      activeTraces += EnterExit
    case  Checkpoint =>
      activeTraces ++= Seq(EnterExit, Checkpoint)
    case  JsonLogs =>
      activeTraces ++= Seq(EnterExit, JsonLogs)
    case  PrintLogs =>
      activeTraces ++= Seq(EnterExit, PrintLogs)
  }


}

trait VisualTracer extends EnableTrace { self =>
  // import VisualTracer._
  import VisualTraceGlobals._

  lazy val tracer = self

  def isEnabled(v: VisualTraceLevel): Boolean = {
    activeTraces.contains(v)
  }

  def traceLevels(): Seq[VisualTraceLevel] = activeTraces.toSeq

  def tracingEnabled(): Boolean = {
    activeTraces.nonEmpty
  }


  def ifTrace(vtl: VisualTraceLevel)(body: => Unit): Unit = macro VisualTraceMacros.runOnTraceLevel[TraceLog]

  def enter()(implicit enclosing: sourcecode.Name): Unit = ifTrace(VisualTraceLevel.EnterExit){
    println(s"entered: ${enclosing.value}")
  }

  def exit()(implicit enclosing: sourcecode.Name): Unit = ifTrace(VisualTraceLevel.EnterExit) {
    println(s"exit: ${enclosing.value}")
  }

  // def Traced[T](body: => T): T = macro VisualTraceMacros.tracedImpl[T]


  def jsonLog(body: => Unit) = ifTrace(VisualTraceLevel.JsonLogs)(body)

  // def jsonAppend(body: => JsObject)(implicit
  //   encFunction: sourcecode.Enclosing,
  //   pageNum: Int@@PageNum
  // ) = ifTrace(VisualTraceLevel.JsonLogs) {
  //   appendLog(l.logname, body)
  // }

  // private def appendLog(pageNum: Int@@PageNum, logname: String, log: json.JsObject): Unit = {
  //   VisualTracer.appendLog(logname, log)
  // }

  def printLog(body: => Unit) = ifTrace(VisualTraceLevel.PrintLogs)(body)


  // def checkpoint(msg: String, args: Any*)(implicit
  //   enclosing: sourcecode.Name,
  //   loc: sourcecode.Enclosing
  // ): Unit = ifTrace(VisualTraceLevel.Checkpoint) {
  //   // println(s"checkpoint@${enclosing.value}/${loc.value}:  '($msg)'")
  // }


  // def createFnLog(implicit
  //   enclosing: sourcecode.Enclosing
  // ): LogSpec = {
  //   val logname = enclosing.value.split("\\.").toList.last
  //   createLog(logname)
  // }

  // def createLog(logname: String): LogSpec = {
  //   VisualTracer.createLog(logname)
  //   LogSpec(logname)
  // }


}
