package edu.umass.cs.iesl.watr
package table  

trait JargonDictionaries {
  import scala.util.matching.Regex


  val acceptableWordPatterns = Seq[Regex](
    """[A-Z]\.[A-Z]""".r,
    """\d+(\.(\d+))*""".r,
    """\d+[-—–]\d+""".r
  )

  val supplementalDict: Set[String] = (
    """|backscatter
       |colloidal
       |compressive
       |crystallites
       |ellipsoidal
       |Elsevier
       |in-situ
       |IPF
       |nucleation
       |mortem
       |reorientation
       |serrations
       |slurry
       |Springer
       |topologies
       |transversal
       |MATLAB
       |TEM
       |CCD
       |CENIM-CSIC
       |EBSD
       |ECAP
       |SEM
       |FEI
       |HCl
       |IMDEA
       |XRD
       |""".stripMargin.split("\n").map(_.trim).toSet
  )

  val prefixes = Seq[String](
    "mis",
    "micro",
    "nano",
    "trans",
    "uni",
    "poly",
    "post",
    "pre",
    "electro"
  )
}
