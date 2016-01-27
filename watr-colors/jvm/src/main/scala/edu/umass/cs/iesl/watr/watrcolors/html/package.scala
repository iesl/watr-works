package edu.umass.cs.iesl.watr
package watrcolors

trait ScalatagsDefs {
  import scalatags.Text
  import scalatags.text

  object texttags
      extends Text.Cap
      with Text.Util
      with Text.Aggregate
      with scalatags.DataConverters


  type TextTag = Text.TypedTag[String]
  type TextModifier = Text.Modifier

  val TextTag = Text.TypedTag

  object < extends Text.Cap with text.Tags with text.Tags2 {
    import texttags._

    def nbsp = raw("&nbsp;")


  }
  object ^ extends Text.Cap with Text.Attrs {

    val `read-more` = "read-more".attr
    val `read-less` = "read-less".attr

  }
  object $ extends Text.Cap with Text.Styles with Text.Styles2

}

package object html extends ScalatagsDefs {


}
