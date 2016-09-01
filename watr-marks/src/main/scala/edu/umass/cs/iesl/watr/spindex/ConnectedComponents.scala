package edu.umass.cs.iesl.watr
package spindex

import scalaz.@@
import watrmarks._

import IndexShapeOperations._
import GeometricFigure._

import TypeTags._
import utils.VisualTracer._
import watrmarks.{StandardLabels => LB}
import scalaz.Tree

/*

 Connected components represent a (rectangular) query against a spatial index

 PageAtoms are the smallest rectangular blocks extracted from PDFs.
 PageComponent is the query that selects a single PageAtom
 RegionComponent is the query that selects everything within its boundary (whitespace and/or PageComponent)


 child/descendant methods on Component should go away, as there is no strict hierarchical structure to CCs

 Instead of child/desc methods, we introduce query types, which select everything within a given region,
   filtered by predicates, including labels on regions, and query modifiers such as "contains|intersects|isInside|etc.."


 Ordering of components within a region (e.g., char/word ordering in a visual line, or line ordering within text blocks),
 is a function of the containing component, e.g., a text block region imposes an ordering on all contained line regions, with
 a vertical increasing Y ordering (implicit), or a link to an explicit ordering

 */

object Query {
  sealed trait Filter
  sealed trait Type
  case object Intersects extends Type
  case object Contains extends Type
}

sealed trait Quantifier
object Quantifier {
  case object All extends Quantifier
  case object Exists extends Quantifier
}


sealed trait Component {
  def id: Int@@ComponentID

  def zoneIndex: ZoneIndexer
  def vtrace = zoneIndex.vtrace

  def roleLabel: Label

  def queryAtoms(): Seq[PageComponent] = {
    queryInside(LB.PageAtom).map(_.asInstanceOf[PageComponent])
  }

  def queryInside(role: Label): Seq[Component] = {
    getChildTree(role)
      .getOrElse ({
        val pinfo = zoneIndex.getPageInfo(zoneIndex.getPageForComponent(this))
        val bounds = this.targetRegion.bbox
        pinfo.componentIndex
          .queryForContained(bounds)
          .filter(_.roleLabel == role)
      })
  }

  def groupAtomsIf(
    groupf: (PageComponent, PageComponent, Int) => Boolean,
    onGrouped: (RegionComponent, Int) => Unit = ((_, _) => ())
  ): Seq[RegionComponent]

  def splitAtomsIf(
    splitf: (PageComponent, PageComponent, Int) => Boolean,
    onSplit: (RegionComponent, Int) => Unit = ((_, _) => ())
  ): Seq[RegionComponent]


  def targetRegions: Seq[TargetRegion]

  // TODO target region only makes sense for some connected components
  def targetRegion: TargetRegion = {
    targetRegions.headOption
      .map(tr => TargetRegion(RegionID(0), tr.target, bounds))
      .getOrElse {  sys.error("no target region found in Component}") }
  }

  def chars: String

  def getChildren(): Seq[Component] = {
    getChildren(roleLabel)
  }

  // TODO: this should really return a Seq[Option[Component]] to signify existance vs. empty children
  def getChildren(l: Label): Seq[Component] = {
    getChildTree(l).getOrElse {Seq()}
  }


  def getChildTree(l: Label): Option[Seq[Component]] = {
    zoneIndex.getChildTree(this, l)
  }

  def setChildTree(l: Label, tree: Seq[Component]): Unit = {
    zoneIndex.setChildTreeWithLabel(this, l, tree.map(_.id))
  }

  def connectChildren(l: Label, sortf: Option[((Component)=>Double)]): Unit = {
    val sub = queryInside(l)
    val sorted = sortf
      .map(sub.sortBy(_))
      .getOrElse(sub)
      .map(_.id)

    zoneIndex.setChildTreeWithLabel(this, l, sorted)
  }


  // def connectChildrens(l: Label, cs: Seq[Component]): Unit = {
  //   zoneIndex.setChildTreeWithLabel(this, l, cs.map(_.id))
  // }

  // def mapChars(subs: Seq[(Char, String)]): Component

  // def toText(implicit idgen:Option[CCRenderState] = None): String
  // def toText(): String

  // TODO: This is redundant w/targetregion
  def bounds: LTBounds

  def orientation: Double = 0.0d // placeholder until this is implemented for real

  import utils.VisualTracer._

  def addLabel(l: Label): Component = {
    vtrace.trace("addLabel" withTrace link(showComponent(this), showLabel(l)))
    zoneIndex.addLabel(this, l)
  }

  def removeLabel(l: Label): Component = {
    vtrace.trace("removeLabel())" withTrace link(showComponent(this), showLabel(l)))
    zoneIndex.removeLabel(this, l)
  }

  def getLabels(): Set[Label] = {
    zoneIndex.getLabels(this)
  }

  def setChildren(l: Label, cs: Seq[Component]): Unit = {
    setChildTree(l, cs)
  }

  def toRoleTree(roles: Label*): Tree[Component] = {
    val children = for {
      r <- roles
    } yield getChildren(r)

    val roleTree = children
      .filterNot(_.isEmpty)
      .headOption
      .map({ c =>
        val cf = c.map(_.toRoleTree(roles:_*))
        Tree.Node(this, cf.toStream)
      })
      .getOrElse({
        Tree.Leaf(this)
      })

    roleTree
  }

}



case class RegionComponent(
  id: Int@@ComponentID,
  override val roleLabel: Label,
  var region: TargetRegion,
  override val zoneIndex: ZoneIndexer
) extends Component {

  def cloneAndNest(l: Label): RegionComponent = {
    val clone = cloneAs(l)
    this.setChildren(l, Seq(clone))
    clone
  }
  def cloneAs(l: Label): RegionComponent = {
    val atoms = queryAtoms()
    val clone = copy(
      id = zoneIndex.componentIdGen.nextId,
      roleLabel = l,
      region = region.copy(
        id = zoneIndex.regionIdGen.nextId
      )
    )
    zoneIndex.addComponent(clone)
    clone.setChildTree(LB.PageAtom, atoms)
    clone
  }

  def groupAtomsIf(
    groupf: (PageComponent, PageComponent, Int) => Boolean,
    onGrouped: (RegionComponent, Int) => Unit = ((_, _) => ())
  ): Seq[RegionComponent] = {

    ???
  }

  def splitAtomsIf(
    splitf: (PageComponent, PageComponent, Int) => Boolean,
    onSplit: (RegionComponent, Int) => Unit = ((_, _) => ())
  ): Seq[RegionComponent] = {
    import utils.SlicingAndDicing._

    def cbounds(cs: Seq[Component]) = {
      cs.map(_.bounds)
        .reduce(_ union _)
    }

    val splitAtoms = queryAtoms().splitOnPairsWithIndex(splitf)

    val splitRegions = splitAtoms
      .zipWithIndex
      .map({case (atoms, regionIndex) =>
      val rc = cloneAs(roleLabel)
      val split = rc.copy(
        region = rc.region.copy(
          bbox = cbounds(atoms)))

      split.setChildren(LB.PageAtom, atoms)
      onSplit(split, regionIndex)
      split
    })

    splitRegions
  }

  def targetRegions: Seq[TargetRegion] = Seq(region)

  def bounds: LTBounds = region.bbox

  def chars: String = {
    queryAtoms().map(_.char).mkString
  }

  def extendRegion(r: TargetRegion): Unit = {
    region = region.copy(bbox = region.bbox union r.bbox)
    vtrace.trace(message(s"extendRegion: ${this.toString()}"))
  }

  override def toString(): String = {
    val lls = getLabels.mkString(",")
    s"<${roleLabel.key}.${id} ${region}${lls}>"
  }
}


case class PageComponent(
  id: Int@@ComponentID,
  component: PageAtom,
  override val zoneIndex: ZoneIndexer
) extends Component {

  def splitAtomsIf(
    splitf: (PageComponent, PageComponent, Int) => Boolean,
    onSplit: (RegionComponent, Int) => Unit = ((_, _) => ())
  ): Seq[RegionComponent] = { Seq() }

  def groupAtomsIf(
    groupf: (PageComponent, PageComponent, Int) => Boolean,
    onGrouped: (RegionComponent, Int) => Unit = ((_, _) => ())
  ): Seq[RegionComponent] = {

    ???
  }



  def roleLabel: Label = LB.PageAtom

  def targetRegions: Seq[TargetRegion] = Seq(component.region)

  def char = component match {
    case rg: CharAtom => rg.char.toString
    case rg: ImgAtom => ""
  }

  val bounds = component.region.bbox

  def chars: String = char

  override def toString(): String = {
    val lls = getLabels.mkString(",")
    s"<`${chars}`${id} ${component.region}$lls>"
  }
}
