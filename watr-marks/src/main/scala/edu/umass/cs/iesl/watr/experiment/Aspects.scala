package edu.umass.cs.iesl.watr

import org.aspectj.lang._
import org.aspectj.lang.annotation._

import textboxing.{TextBoxing => TB}, TB._

object Global {
  var indent: Int = 1
}

@Aspect
class WatrAspects {

  // @Pointcut("call(* toAtoms(..))")
  // def pcToAtoms() =  {}

  // @Before("pcToAtoms()")
  // def beforePcToAtoms(jp: JoinPoint): Unit = {
  //   println(s"beforePcToAtoms:${jp}")
  // }

  // @Pointcut("within(matryoshka..*)")
  // @Pointcut("execution(* matryoshka..*.*(..))")
  // @Pointcut("call(* matryoshka..*.*(..)) && !within(WatrAspects)")
  // def pointcut0() = {
  // }

  // @Before("pointcut0()")
  // def before_pointcut0(jp: JoinPoint) = println(s"before:pointcut0 ${jp}")


  // @Pointcut("withincode(_root_.matryoshka..*.*(..)) && execution(* *(..)) && !within(WatrAspects)")
  // @Pointcut("cflow(pointcut0()) && execution(* *(..))")

  @Pointcut("""
!cflow(execution(String *.toString())) &&
!cflow(call(String *.toString())) &&
!within(edu.umass.cs.iesl.watr.textboxing..*) &&
!call(* scala..*.*(..)) &&
!call(* java..*.*(..)) &&
!within(edu..*.Global$) &&
!within(org.aspectj..*.*) &&
!within(WatrAspects) && (
  execution(* *(..))  ||
  call(* *(..))
)
""")
  def pointcut1() = {
  }


  @Around("pointcut1()")
  def around_pointcut1(jp: ProceedingJoinPoint): Any = {
    val ind = " " * Global.indent*2

    val stp = jp.getStaticPart



    // val sourceLoc = jp.getSourceLocation
    // val srcLoc =
    //   s"""|SourceLoc
    //       | getFileName    ${sourceLoc.getFileName  }
    //       | getLine        ${sourceLoc.getLine      }
    //       | getWithinType  ${sourceLoc.getWithinType}
    //       |""".stripMargin
    val args = jp.getArgs.map(_.toString()).mkString(", ")
    println(s"${ind}${jp} ${args}")
    // println(
    //   indent(Global.indent*2)(
    //     s"""|StaticPart
    //         |    getId             ${stp.getId              }
    //         |    getKind           ${stp.getKind            }
    //         |    getSignature      ${stp.getSignature       }
    //         |    getSourceLocation ${stp.getSourceLocation  }
    //         |    toLongString      ${stp.toLongString       }
    //         |    toShortString     ${stp.toShortString      }
    //         |    toString          ${stp.toString           }
    //         |""".stripMargin.mbox
    //   )
    // )
    // val sig = jp.getSignature
    // val sigStr =
    //   s"""|Signature
    //       |  getDeclaringType    ${ sig.getDeclaringType     }
    //       |  getDeclaringTypeName${ sig.getDeclaringTypeName }
    //       |  getModifiers        ${ sig.getModifiers         }
    //       |  getName             ${ sig.getName              }
    //       |  toLongString        ${ sig.toLongString         }
    //       |  toShortString       ${ sig.toShortString        }
    //       |""".stripMargin

    // println(
    //   s"""|
    //       |getArgs           ${ args           }
    //       |getKind           ${ jp.getKind           }
    //       |${sigStr}
    //       |getSourceLocation ${ jp.getSourceLocation }
    //       |${srcLoc}
    //       |getStaticPart     ${ jp.getStaticPart     }
    //       |toLongString      ${ jp.toLongString      }
    //       |toShortString     ${ jp.toShortString     }
    //       |""".stripMargin
    // )
    Global.indent += 1
    val ret = jp.proceed()
    Global.indent -= 1
    println(s"${ind}^ ${ret}")
    ret

  }

  // @Before("pointcut1()")
  // def before_pointcut1(jp: JoinPoint) = {

  //   println(s"before:pointcut1 ${jp}")

  //   val args = jp.getArgs.map(_.toString()).mkString(", ")
  //   val sig = jp.getSignature
  //   // () => Class[?0]
  //   //   () => String
  //   // () => Int
  //   // () => String
  //   // () => String
  //   // () => String

  //   val sigStr =
  //     s"""|Signature
  //         |  getDeclaringType    ${ sig.getDeclaringType     }  () => Class[?0]
  //         |  getDeclaringTypeName${ sig.getDeclaringTypeName }    () => String
  //         |  getModifiers        ${ sig.getModifiers         }  () => Int
  //         |  getName             ${ sig.getName              }  () => String
  //         |  toLongString        ${ sig.toLongString         }  () => String
  //         |  toShortString       ${ sig.toShortString        }  () => String
  //         |""".stripMargin

  //   // |getSignature      ${ jp.getSignature      }  () => Signature
  //   println(
  //     s"""|
  //         |getArgs           ${ args           }  () => Array[Object]
  //         |getKind           ${ jp.getKind           }  () => String
  //         |${sigStr}
  //         |getSourceLocation ${ jp.getSourceLocation }  () => SourceLocation
  //         |getStaticPart     ${ jp.getStaticPart     }  () => StaticPart
  //         |toLongString      ${ jp.toLongString      }  () => String
  //         |toShortString     ${ jp.toShortString     }  () => String
  //         |""".stripMargin
  //   )
  // }

  // @After("pointcut1()")
  // def after_pointcut1(jp: JoinPoint) =  {
  //   println(s"after:pointcut1 ${jp}")
  // }

  // @Before("pointcut1()")
  // def trace_in_pointcut1(jp: JoinPoint) = {
  // }

  // @After("pointcut1()")
  // def trace_out_pointcut1(jp: JoinPoint) =  {
  //   println(s"after:pointcut1 ${jp}")
  // }
}



object AspectMain {

  Global.indent=1

  import watrmarks.{StandardLabels => LB}
  import textflow._
  import TextReflow._
  import matryoshka._
  import matryoshka.data._

  def toAtoms(s: String): List[TextReflow] = {
    s.toList.map(_ match {
      case ' ' => space()
      case c   => atom(c, new TextReflowAtomOps(Seq(c)))
    })
  }

  import scalaz._, Scalaz._
  import TextReflowOps._

  def main(args: Array[String]) = {


    def `Eu1-x` = flow(
      flows(toAtoms("Eu")),
      labeled(LB.Sub, flows(toAtoms("1 - x")))
    )

    val ranges = `Eu1-x`.annotateCharRanges()

    // println(prettyPrintTree(f0))
    // println(printCofree(ranges))
    // val hidden = `Eu1-x`.modifyCharAt(6)(hideChar)
    // val rewritten = `Eu1-x`.modifyCharAt(6)(
    //   (c: Char, index: Int)  => None: Option[Char]
    // )

  }
}
