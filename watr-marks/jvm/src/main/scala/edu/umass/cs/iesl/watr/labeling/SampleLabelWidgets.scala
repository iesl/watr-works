package edu.umass.cs.iesl.watr
package labeling

import textboxing.{TextBoxing => TB}, TB._
import geometry._
import LabelWidgets._
import LabelWidgetF._
import utils.Colors
import utils.EnrichNumerics._
import TypeTags._

object SampleLabelWidgets {

  def dimensionTest(): LabelWidget = {
    val sampleText1 = textbox(
      vjoin(center1)(
        "aliqua. Ut enim ad minim veniam, ",
        "incididunt ut labore et dolore magna ",
        "aliqua. Ut enim ad minim veniam, ",
        "Anyconsectetur adipisicing elit, sed do eiusmod tempor"
      )
    )

    val sampleText2 = textbox(
      vjoin(left)(
        "Lorem ipsum dolor sit amet",
        "Anyconsectetur adipisicing elit, sed do eiusmod tempor",
        "aliqua. Ut enim ad minim veniam, "
      )
    )

    val panel0 = panel(
      sampleText1,
      LabelAction.clickToSelectZone(ZoneID(23))
    )
    val panel1 = panel(
      sampleText1,
      LabelAction.clickToSelectZone(ZoneID(90))
    )

    val r0 = row(
      pad(panel0, Padding(3), Colors.LightBlue),
      pad(panel1, Padding(9), Colors.Azure)
    )

    val col0 = col(
      r0,
      pad(r0, Padding(2), Colors.Gray(3.percent))
    )
    val layout = pad(col0, Padding(10), Colors.Red)

    layout

  }


  // select page region
  /*

   Final goal:
     Indicate one or more selected region corresponding to a zone (e.g., Abstract)
     Create controls to delete zone
     Allow selection/de-selection of zone via indicator


   Test case sample:
     Create textbox-backed pages (or PlainText reflow)
     Make each page clickable to select zone at point
     Create a bounding region around lines when clicked
     Control bar: Delete, add/mod label, deselect
     Make page respond to SelectRegion
     Emboss lines w/labels to indicate they are clickable

   val page1 =..

   def button() = panel(figure(ltbbox), Action.Click => {...})
   def radioButtons() = row(button())
   val delButton() = button()

   val zoneIndicatorControlBar = panel(
     row(
       labelSelectors, currLabel, mergeButton, delButton
     ),
     actions=List(

     )
   )

   overlay(
     page1,
     panel(
       Action.Click => {chooseGuessedLabel() || selectZoneAtPoint()},
       Action.SelectRegion => {bbox => val z = createZone(); selectZone(z); }
     )

   )


   button(
     "delete", 'X',
     (zoneId) => deleteZone(zoneId))
   )
   panel(
     Action.Click => {selectZoneAtPoint()}
   )
   panel(
     Action.Click => {chooseLabel || selectZoneAtPoint()}
   )
   Action.DblClick => {}
   Action.RightClick => {}

   Action.Select => {bbox => selectZoneAtPoint()}


   - Action types
   - Panel: Mouse interaction region
   - Button(Toggle/Radio/etc): Panel specialized for clicks/presses
   - GeometricFigure primitive
   - Overlay combinator






   */

}
