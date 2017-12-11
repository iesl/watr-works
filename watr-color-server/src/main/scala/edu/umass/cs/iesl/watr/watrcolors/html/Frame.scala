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

      <.script("text/javascript".typ, ^.src := "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"),
      <.script("text/javascript".typ, ^.src := "https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"),

      <.script("text/javascript".typ, ^.src := "https://cdnjs.cloudflare.com/ajax/libs/jquery-serialize-object/2.5.0/jquery.serialize-object.min.js"),

      <.link(^.rel:="stylesheet", ^.href:="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/themes/smoothness/jquery-ui.css"),
      <.link(^.rel:="stylesheet",
        ^.href:=" https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
        "integrity".attr:="sha384-wvfXpqpZZVQGK6TAh5PVlGOfQNHSoD2xbE+QkPxCAFlNEevoEH3Sl0sibVcOQVnN",
        "crossorigin".attr:="anonymous"),

      <.script("text/javascript".typ, ^.src := "https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.4/lodash.min.js"),

      <.script("text/javascript".typ, ^.src := "https://cdnjs.cloudflare.com/ajax/libs/d3/4.12.0/d3.min.js"),

      <.script("text/javascript".typ, ^.src := "https://cdnjs.cloudflare.com/ajax/libs/rxjs/5.5.5/Rx.min.js"),

      <.title("WatrColors")
    )
  }


  def apply(bundleName: String) = {
    empty()
  }

  def withBodyContent(content: TextTag) = {
    <.html(
      htmlHead(),
      <.body(
        content,
        <.script("text/javascript".typ, ^.src := s"/dist/app.bundle.js")
      ),
    )
  }
  def empty() = {
    <.html(
      htmlHead(),
      <.body(
        <.script("text/javascript".typ, ^.src := s"/dist/app.bundle.js")
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

}


object Authentication {
  def labeledPasswordInput(label: String, key: String)  = {
    <.div(
      <.input("password".typ, key.name, key.id),
      <.label(^.`for` := key, label)
    )
  }

  def labeledTextInput(label: String, key:String)  = {
    <.div(
      <.input("text".typ, key.name, key.id),
      <.label(^.`for` := key, label)
    )
  }

  def loginForm()  = {
    val forms = <.div("login-forms".id, ^.hidden := true,
      "Login",
      <.form(^.action := "/api/v1/auth/login", ^.method := "POST",
        ^.enctype:= "application/x-www-form-urlencoded",
        <.div(
          labeledTextInput("Email", "email"),
          labeledPasswordInput("Password", "password"),
          <.div(
            <.button("submit".typ, "Login".value, "Login")
          )
        ),
      ),
      "Or Signup",
      <.form(^.action:= "/api/v1/auth/signup", ^.method := "POST",
        ^.enctype:= "application/x-www-form-urlencoded",

        <.div(
          labeledTextInput("Email", "email"),
          labeledTextInput("Username", "username"),
          labeledPasswordInput("Password", "password"),
          <.div(
            <.button("submit".typ, "Signup".value, "Signup")
          )
        )
      )
    )

    Frame.withBodyContent(
      forms
    )
  }

}
