package edu.umass.cs.iesl.watr
package tracing

import tracemacros._
import scala.language.experimental.macros
import geometry._

// import textboxing.{TextBoxing => TB}

import play.api.libs.json, json._

object VisualTracer {
  import scala.collection.mutable
  import VisualTraceLevel._


  val activeTraces = mutable.HashSet[VisualTraceLevel]()

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

  // val jsonLogs = mutable.ListBuffer[JsObject]()
  val jsonLogs = mutable.HashMap[String,
    mutable.ArrayBuffer[
      mutable.ArrayBuffer[JsObject]
    ]
  ]()

  def clearAllLogs(): Unit = {
    jsonLogs.clear()
  }

  def createLog(logname: String): Unit = {
    assume(!jsonLogs.contains(logname))
    val logsBuffer = jsonLogs.getOrElseUpdate(logname,
      mutable.ArrayBuffer()
    )
    logsBuffer += mutable.ArrayBuffer()
  }

  def appendLog(logname: String, log: json.JsObject): Unit = {
    assume(jsonLogs.contains(logname))

    jsonLogs.get(logname).foreach { logBuffers =>
      val currLogs = logBuffers.last
      currLogs += log
    }
  }

  def emitLogs(): JsValue = {
    val allLogs = for {
      (logname, logInstances) <- jsonLogs
      logInstance <- logInstances
    } yield {

      Json.obj(
        ("name", logname),
        ("steps", logInstance)
      )

    }

    Json.toJson(allLogs)
  }

}


trait VisualTracer { self =>
  import VisualTracer._

  lazy val tracer = self

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


  def jsonLog(body: => Unit) = ifTrace(VisualTraceLevel.JsonLogs)(body)

  def jsonAppend(body: => JsObject)(implicit l: LogSpec) =
    ifTrace(VisualTraceLevel.JsonLogs) {
      appendLog(l.logname, body)
    }

  def printLog(body: => Unit) = ifTrace(VisualTraceLevel.PrintLogs)(body)


  def checkpoint(msg: String, args: Any*)(implicit
    enclosing: sourcecode.Name,
    loc: sourcecode.Enclosing
  ): Unit = ifTrace(VisualTraceLevel.Checkpoint) {
    println(s"checkpoint@${enclosing.value}/${loc.value}:  '($msg)'")
  }

  def createLog(logname: String): LogSpec = {
    VisualTracer.createLog(logname)
    LogSpec(logname)
  }

  def appendLog(logname: String, log: json.JsObject): Unit = {
    VisualTracer.appendLog(logname, log)
  }

}

case class LogSpec(
  logname: String
)
