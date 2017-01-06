package edu.umass.cs.iesl.watr
package watrcolors
package html

import scalatags.Text.all._

import corpora._

class SvgOverviewView() {


  def containerFluid = `class` := "container-fluid"
  def row = `class` := "row"
  def col(n: Int) = `class` := s"col-md-$n"

  def initPageImages(corpusEntry: CorpusEntry): TextTag = {

    val imgList = for {
      pageImages <- corpusEntry.getArtifactGroup("page-images")
      imgArtifacts = pageImages.getArtifacts
      sortedArtifacts = {
        imgArtifacts.sortBy { a =>
          // dirty hack to make sure we get the images in order
          val name = a.artifactPath.name
          val imgNumber = name.dropWhile(!_.isDigit).takeWhile(_.isDigit).mkString.toInt
          imgNumber
        }
      }
    } yield for {
      image <- sortedArtifacts
      path <- image.asPath.toOption.toSeq
    } yield {
      val src = s"/repo/${image.descriptor}"

      <.img(^.src := src, ^.`class`:="page-image", WatrStyles.pageImg)
    }

    div(containerFluid)(
      <.style(^.`type`:="text/css", WatrStyles.styleSheetText),
      <.style(^.`type`:="text/css",""".hover { background-color: green; }"""),
      <.div(row)(
        <.div(col(6))(
          <.div(^.id:="overlay-container", WatrStyles.overlayContainer)(
            <.canvas(^.id:="fabric-canvas", WatrStyles.fabricCanvas),
            <.div(^.id:="img-container", WatrStyles.imgContainer)(
              imgList
            )
          )
        ),

        <.div(col(6))(
          <.div(WatrStyles.infoPane)(
            <.span("Info Area"),
            <.ul(^.id:="messages", ^.style:="")
          )
        )
      )
    )

}


// def init(corpusEntryId: String)  = {

//   div(containerFluid)(
//     <.style(^.`type`:="text/css", SvgStyles.styleSheetText),
  //     <.div(row)(
  //       <.div(col(6))(
  //         <.div(^.id:="overlay-container", SvgStyles.overlayContainer)(
  //           <.div(^.id:="svg-container", SvgStyles.svgImage),
  //           <.canvas(^.id:="fabric-canvas", SvgStyles.fabricCanvas),
  //           <.script(^.`type`:="text/javascript")(
  //             raw(s"""|var documentIconURI = "images/pdf_50x50.png";
  //                     |var corpusEntryId = "$corpusEntryId";
  //                     |""".stripMargin)),
  //           <.script(^.`type`:="text/javascript", ^.src:="/assets/js/edit-document.js" )
  //         )
  //       ),

  //       <.div(col(6))(
  //         <.div(SvgStyles.infoPane)(
  //           <.span("Info Area"),
  //           <.span(^.id:="rxinfo"),
  //           <.span(^.id:="selection-info")
  //         )
  //       )
  //     )
  //   )
  // }
}
