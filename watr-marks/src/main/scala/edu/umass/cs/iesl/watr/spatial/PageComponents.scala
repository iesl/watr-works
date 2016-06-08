package edu.umass.cs.iesl.watr
package spatial

package plusplus

import watrmarks.Label

sealed trait Component {
}

case class LeafComponent(
  component: PageComponent,
  blockRole: Option[Label] = None
) extends Component {
}

case class ConnectedComponent(
  components: Seq[Component],
  blockRole: Option[Label] = None
) extends Component {

}
