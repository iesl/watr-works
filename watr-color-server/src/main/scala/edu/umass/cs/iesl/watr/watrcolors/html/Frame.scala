package edu.umass.cs.iesl.watr
package watrcolors
package html

import texttags._
import watrmarks._

object Frame {
  def htmlHead() = {
    <.head(
      <.meta(^.name := "viewport", ^.content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
      <.meta(^.httpEquiv:="Content-Type", ^.content:="text/html"),
      <.meta(^.httpEquiv:="X-UA-Compatible", ^.content:="IE=edge"),
      <.meta(^.charset:="utf-8"),
      <.title("WatrColors"),
    )
  }

  def apply(bundleName: String) = {
    <.html(
      htmlHead(),
      <.body(
        <.script("text/javascript".typ, ^.src := s"/dist/${bundleName}.bundle.js")
      ),
    )
  }

}

object Parts {
  def modalSkeleton(
    idAttr: String,
    header: TextTag,
    body: TextTag,
    footer: Option[TextTag]
  ): TextTag = {

    <.div(^.`class`:="modal fade", idAttr.id, ^.`tabindex`:="-1", ^.`role`:="dialog", "aria-labelledby".attr := s"${idAttr}Label", ^.aria.hidden:="true")(
      <.div(^.`class`:="modal-dialog", ^.`role`:="document")(
        <.div(^.`class`:="modal-content")(
          <.div(^.`class`:="modal-header")(
            header
          ),
          <.div(^.`class`:="modal-body")(
            body,
          ),
          footer.map{ f =>
            <.div(^.`class`:="modal-footer")(
              footer
            )
          }
        )
      )
    )
  }

  def labelButton(label: Label): TextTag = {
    <.button(
      "labelChoice".name,
      label.fqn.value,
      label.fqn.id,
      "labelChoice btn btn-xs btn-block btn-default".clazz,
      "submit".typ
    )(<.small(label.fqn))
  }

  def modalDialogTitle(title: String): TextTag = {
    <.h5(^.`class`:="modal-title", ^.`id`:="")(title)
  }

  def labelingForm(labels: Seq[Label]): TextTag = {
    val buttons = labels.map(labelButton(_))

    <.form(^.enctype:="multipart/form-data")(
      <.input("hidden".typ, "selectedLabel".id, "selectedLabel".name),
      <.div(^.`class`:="form-group")(
        buttons
      )
    )
  }

  def labelingPanel(labels: Seq[Label]): TextTag = {
    val form = labelingForm(labels)

    val postForm = form(
      "/api/v1/label/region".action,
      "POST".method
    )

    modalSkeleton("label-chooser", modalDialogTitle("Choose label"), postForm, None)
  }

  // def labelingPanelXX(): TextTag = {

  //   <.div(^.`class`:="modal fade", ^.`id`:="exampleModal", ^.`tabindex`:="-1", ^.`role`:="dialog", "aria-labelledby".attr :="exampleModalLabel", ^.aria.hidden:="true")(
  //     <.div(^.`class`:="modal-dialog", ^.`role`:="document")(
  //       <.div(^.`class`:="modal-content")(
  //         <.div(^.`class`:="modal-header")(
  //           <.h5(^.`class`:="modal-title", ^.`id`:="exampleModalLabel")("New message"),
  //           <.button(^.`type`:="button", ^.`class`:="close", "data-dismiss".attr:="modal", ^.aria.label:="Close")(
  //             <.span(^.aria.hidden:="true")("x")
  //           )
  //         ),
  //         <.div(^.`class`:="modal-body")(
  //           <.form(
  //             <.div(^.`class`:="form-group")(
  //               <.label(^.`for`:="recipient-name", ^.`class`:="col-form-label")("Recipient:"),
  //               <.input(^.`type`:="text", ^.`class`:="form-control", ^.`id`:="recipient-name")(
  //               ),
  //               <.div(^.`class`:="form-group")(
  //                 <.label(^.`for`:="message-text", ^.`class`:="col-form-label")("Message:"),
  //                 <.textarea(^.`class`:="form-control", ^.`id`:="message-text")()
  //               )
  //             )
  //           ),
  //           <.div(^.`class`:="modal-footer")(
  //             <.button(^.`type`:="button", ^.`class`:="btn btn-secondary", ^.data.dismiss:="modal")("Close"),
  //             <.button(^.`type`:="button", ^.`class`:="btn btn-primary")("Send message")
  //           )
  //         )
  //       )
  //     )
  //   )
  // }


}
