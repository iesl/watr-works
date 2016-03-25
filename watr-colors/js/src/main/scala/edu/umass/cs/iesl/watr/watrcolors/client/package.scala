package edu.umass.cs.iesl.watr
package watrcolors

// TODO factor out commonalities between ScalatagsDomDefs and ScalatagsDomDefs
import scalatags.jsdom
import scalatags.JsDom

trait ScalatagsDomDefs {

  object domtags
      extends JsDom.Cap
      with JsDom.Util
      with JsDom.Aggregate
      with scalatags.DataConverters


  type JsDomTag = JsDom.TypedTag[String]
  type JsDomModifier = JsDom.Modifier

  val JsDomTag = JsDom.TypedTag

  object < extends JsDom.Cap with jsdom.Tags with jsdom.Tags2 with jsdom.SvgTags {
    import domtags._

    def nbsp = raw("&nbsp;")

  }

  object ^ extends JsDom.Cap with JsDom.Attrs {
    lazy val x = "x".attr
    lazy val y = "y".attr
    lazy val width = "width".attr
    lazy val height = "height".attr
    lazy val labelName = "label-name".attr
    lazy val labelValue = "label-value".attr

  }

  object $ extends JsDom.Cap with JsDom.Styles with JsDom.Styles2

  import domtags._


  implicit class RichString(val s: String)  {
    def clazz = ^.`class` := s
    def id = ^.`id` := s
    def labelName = ^.labelName:=s
    def labelValue = ^.labelValue:=s
    def attrTarget = "target".attr:=s
  }

  def fmt = (d: Double) => f"${d}%1.2f"

  implicit class RichDouble(val v: Double)  {
    def attrX      = ^.x      := fmt(v)
    def attrY      = ^.y      := fmt(v)
    def attrWidth  = ^.width  := fmt(v)
    def attrHeight = ^.height := fmt(v)
  }

}

package object client extends ScalatagsDomDefs {


}
