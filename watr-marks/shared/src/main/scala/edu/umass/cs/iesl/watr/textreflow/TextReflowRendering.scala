package edu.umass.cs.iesl.watr
package textreflow

object TextReflowRendering {
  import TextReflowF._
  import watrmarks.{StandardLabels => LB}

  //  if (l.hasLabel(LB.Sup)) { Bracket("^{", "}",  a) }
  def escapeLineFormatting: TextReflowT => TextReflowT = {
    case l @ Labeled (labels, a)     =>
      val esc = if (l.hasLabel(LB.Sup)) Option("^")
      else if (l.hasLabel(LB.Sub))      Option("_")
      else None

      esc.map(e =>
        flow(insert(s"$e{"), a, insert("}")).unFix
      ) getOrElse (l)

    case t      => t
  }

  def renderText(t: TextReflowF[(TextReflow, String)]): String = t match {
    case Atom    (ac)                    => ac.char
    case Insert  (value)                 => value
    case Rewrite ((from, attr), to)      => to
    case Bracket (pre, post, (a, attr))  => s"$pre${attr}$post"
    // case Mask    (mL, mR, (a, attr))     => attr.drop(mL).dropRight(mR).mkString
    case Flow    (atomsAndattrs)         => atomsAndattrs.map(_._2).mkString
    case Labeled (labels, (a, attr))     => attr
    // case CachedText((a, attr), text)     => text
  }
}
