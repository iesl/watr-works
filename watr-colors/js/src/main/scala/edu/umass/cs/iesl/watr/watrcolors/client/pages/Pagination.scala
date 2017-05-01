package edu.umass.cs.iesl.watr
package watrcolors
package client
package pages

import parts._
import labeling._
import scaladget.stylesheet.{all => sty}
import scalatags.JsDom.all._
import rx._

import BootstrapBits._
import TypeTags._


class Paginator(
  pagination: Pagination
)(implicit co: Ctx.Owner) extends BaseClientDefs {

  val Pagination(pgCount0, currPage0, pgInfo0) = pagination

  val pageCount: Var[Int] = Var(pgCount0)
  val currentPage: Var[Int] = Var(currPage0.unwrap)
  val pageInfo: Var[Option[Seq[PaginationInfo]]] = Var(pgInfo0)

  def currentPagination: Pagination =  {
    Pagination(pageCount.now, PageNum(currentPage.now), pageInfo.now)
  }

  def pageControls: RxModifier = Rx {
    val pageButtons = (0 until pageCount()).map{ pgnum =>
      selectableButton(
        pgnum.toString,
        (pgnum==currentPage()),
        modifierSeq = (sty.btn_small),
        onclick = () => Rx { currentPage() = pgnum }
      )
    }
    radios()(pageButtons:_*).render
  }

}
