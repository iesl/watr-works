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

  // val pageCount: Var[Int] = Var(pgCount)
  // val currentPage: Var[Int] = Var(currPage.unwrap)
  // val pageInfo: Var[Seq[PaginationInfo]] = Var(pgInfo.get)

  val paginationRx: Var[Pagination] = Var(pagination)

  val pageControls: RxModifier = Rx {
    val Pagination(pgCount, currPage, pgInfo) = paginationRx.now


    val pageButtons = (0 until pgCount).map{ pgnum =>
      selectableButton(
        pgnum.toString,
        (pgnum==currPage),
        modifierSeq = (sty.btn_small),
        onclick = () => Rx { paginationRx().copy(currentPage=PageNum(pgnum)) }
      )
    }
    radios()(pageButtons:_*).render
  }

  val getRx = paginationRx
}

// class PaginationState(
//   uiRequestCycle: (UIRequest) => Future[Unit],
//   numPages: Int,
//   currPage: Int,
//   info: Seq[PaginationInfo]
// )(implicit co: Ctx.Owner) extends BaseClientDefs {

//   val pageCount: Var[Int] = Var(numPages)
//   val currentPage: Var[Int] = Var(currPage)
//   val pageInfo: Var[Seq[PaginationInfo]] = Var(info)

//   val paginationControls: RxModifier = Rx {
//     val pg = currentPage()
//     val pgs = pageCount()

//     val pageButtons = (0 until pgs).map{ pgnum =>
//       selectableButton(
//         pgnum.toString,
//         (pgnum==pg),
//         modifierSeq = (sty.btn_small),
//         onclick = () => {currentPage() = pgnum})
//     }
//     radios()(pageButtons:_*).render

//   }

//   val setCurrentPage = currentPage.triggerLater {
//     uiRequestCycle(
//       UIRequest(
//         toUIState,
//         MenuAction(LabelAction.NavigateTo(currentPage.now))
//       )
//     )
//   }
// }
