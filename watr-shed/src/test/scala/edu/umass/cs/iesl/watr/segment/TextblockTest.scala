package edu.umass.cs.iesl.watr

package segment

import scalaz.@@

import spindex._
import extract._

// import segment.SpatialIndexOperations._
// import segment.DocumentSegmenter._
// import Bounds._
// import SpatialIndexOperations._

case class TextblockExample(
  source: String,
  pageId: Int@@PageID,
  regionsWithLabels: Seq[(LTBounds, String)]
)




// Paragraphs/section headings:


// Section heading
   // 1. Introduction --> <svg:rect class="linebox" x="47.28" y="548.31" width="90.75"  height="9.46" />
// paragraph start
// textblock start
   //  Lithium ion batteries (LIBs) have been extensively used in por– --> <svg:rect class="linebox" x="47.06" y="571.07" width="239.30"  height="7.97" />

   // textblock end
   //  alternative anode materials has become an urgent task now– {^{adays.Amongthefeasibleanodematerials,Fe}} --> <svg:rect class="linebox" x="47.28" y="548.40" width="444.22"  height="145.22" />


   // <!--
   // textblock end
   //  charging and discharging at high current densities. --> <svg:rect class="linebox" x="304.27" y="685.87" width="200.90"  height="7.97" />

class TextBlockTest extends DocsegTestUtil  {
  behavior of "block identification and ordering"

  // import Component._
  val testExamples = List(
    // TextblockExample("2839.pdf", page(0),
    TextblockExample("0575.pdf", page(1), Seq())
  )

  // <!--
  //   AND -->
  //   <svg:rect class="linebox" x="100.56" y="220.66" width="16.19"  height="8.21" />
  //   <!--
  //   PETER FISCHER -->
  //   <svg:rect class="linebox" x="123.60" y="219.29" width="80.01"  height="9.99" />

  it should "identify text blocks" in {
    testExamples.foreach{ example =>
      println(s"\n\ntesting ${example.source}")
      val pdfIns = papers.paper(example.source)

      val zoneIndex = ZoneIndexer.loadSpatialIndices(
        DocumentExtractor.extractChars(pdfIns)
      )
      val pages = zoneIndex.getPages.take(1).map({ pageId =>
        val pageChars = zoneIndex.pageInfos(pageId).charAtomIndex.getItems()
        val pageGeom = zoneIndex.getPageGeometry(pageId)
      })



      // val pwidth = totalBounds.width
      // val pheight = totalBounds.height

      // val svgHead = s"""<svg:svg version="1.1" width="${pwidth}px" height="${pheight}px" viewBox="0 0 ${pwidth} ${pheight}" xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">"""

      // (svgHead % totalSvg % "</svg:svg>").toString


      // val docstrum = new DocumentSegmenter(zoneIndex)

      // val allPageLines = for {
      //   pageId <- docstrum.zoneIndexer.getPages
      // } yield {
      //   docstrum.determineLines(pageId, docstrum.zoneIndexer.getAtoms(pageId))
      // }

    }
  }




}

// Document: file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/test-papers/6376.pdf.d/6376.pdf

// Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/test-papers/6376.pdf.d/6376.pdf

// Journal of Alloys and Compounds 383 (2004) 37–39                                         (l:236.50, t:452.42, w:168.39, h:7.17)
// Pressure effect on the Curie temperature of the Heusler                                  (l:148.89, t:500.06, w:373.01, h:15.24)
// alloys Rh_2_MnZ (Z _=_ Sn, Ge)                                                           (l:234.43, t:519.98, w:198.60, h:20.80)
// Y . Adachi ^a^^,^^∗^, H. Morita ^a^, T. Kanomata ^b^, A. Sato ^b^, H. Y oshida ^c^,      (l:166.70, t:548.43, w:337.38, h:15.10)
// T. Kaneko ^c^, H. Nishihara ^d^                                                          (l:263.79, t:564.16, w:139.46, h:14.32)
// ^a^ Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan                (l:214.45, t:587.71, w:240.05, h:8.71)
// ^b^ Faculty of Engineering, Tohoku Gakuin University, Tagajo 985-8537, Japan             (l:210.72, t:597.68, w:247.52, h:8.71)
// ^c^ Institute of Materials Research, Tohoku University, Sendai 980-8577, Japan           (l:211.01, t:607.64, w:246.93, h:8.71)
// ^d^ Faculty of Science and Technology, Ryukoku University, Otsu 520-2123, Japan          (l:206.26, t:617.60, w:256.43, h:8.71)
// Abstract                                                                                 (l:84.33, t:658.31, w:33.36, h:8.07)
// The pressure effect on the Curie temperature (T_C_) of Rh_2_MnZ (Z _=_ Sn, Ge) has been investigated by measuring the temperature dependence                    (l:93.30, t:680.23, w:493.15, h:11.01)
// of initial permeability at various pressures up to about 1 GPa. The Curie temperature T_C_ and its pressure coefﬁcients (1/T_C_)dT_C_/dp were obtained          (l:84.33, t:691.19, w:502.12, h:8.77)
// _i_ t _n_ o _d_ b _u_ e _c_ 4 _e_ 3 _d_ 1 _p_ K _h_ _a_ a _s_ n _e_ d _t_ _r_ _+_ _a_ _n_ 2 _s_ . _i_ 6 _t_ _i_ _×_ _o_ _n_ 1 0 _o_ ^−^ _c_ _c_ ^2^ _u_ G _r_ _s_ P a _a_ ^−^ _r_ _o_ ^1^ _u_ f _n_ o _d_ r R _0_ _._ h _6_ _2_ _G_ M _P_ n _a_ S _i_ n _n_ ,                                               r e s s u r e    (l:84.33, t:700.94, w:502.12, h:20.94)
// © 2004 Elsevier B.V . All rights reserved.                                      (l:84.33, t:724.07, w:147.50, h:8.07)
// Keywords: Heusler alloys; Pressure effect; Curie temperature                    (l:84.33, t:744.84, w:194.57, h:7.17)
// dependence of the exchange interaction in the ferromagnetic                     (l:347.35, t:790.41, w:239.10, h:8.97)
// 1. Introduction                                                                 (l:84.33, t:790.47, w:66.52, h:8.97)
// Mn Heusler alloys.                                                              (l:347.35, t:802.37, w:76.99, h:8.97)
// L 2 ^T^ _1_ - ^h^ t ^e^ y p ^H^ e ^e^ c ^u^ r y ^s^ ^l^ s ^e^ t ^r^ a l ^a^ s ^l^ t ^l^ r ^o^ u ^y^ c ^s^ t u ^R^ r e ^h^ . ^2^ R ^M^ h ^n^ _2_ ^Z^ M n ^(^ S ^Z^ n ^=^ i s f ^S^ e r ^n^ r ^,^ o m ^G^ a ^e^ g ^)^ n ^h^ e t ^a^ i ^v^ c ^e^ w ^t^ i ^h^ t h                                                                (l:84.33, t:812.39, w:239.10, h:21.90)
// the Curie temperature T_C_ of 412K and has a total mag-                        (l:84.33, t:836.30, w:239.10, h:9.94)
// 2. Experimental details                                                        (l:347.35, t:839.46, w:101.23, h:8.97)
// netic moment of 3.1µ _B_/formula [1]. Rh_2_MnGe is also fer-                   (l:84.33, t:847.47, w:239.10, h:10.72)
// romagnetic with T_C_ of 450K and a magnetic moment of                          (l:84.33, t:860.21, w:239.10, h:9.94)
// G e ^T^ ) ^h^ w ^e^ e r ^p^ e ^o^ ^l^ p ^y^ r ^c^ e ^r^ p ^y^ a ^s^ ^t^ r ^a^ e ^l^ d ^l^ ^i^ ^n^ f ^e^ r o m ^s^ ^a^ ^m^ R ^p^ h ^l^ ^e^ ( ^s^ 9 9 ^o^ . ^f^ 9 % ^R^ ) ^h^ , ^2^ M ^M^ n ^n^ ^Z^ ( 9 9 ^(^ ^Z^ . 9 9 % ^=^ ) , ^S^ S ^n^ n ^,^                                                                              (l:347.35, t:863.37, w:239.10, h:20.92)
// 4.3µ _B_/formula [2]. The electronic structure and magnetic                  (l:84.33, t:871.39, w:239.10, h:10.72)
// moment of Rh_2_MnX (X _=_ Ge, Sn and Pb) were calcu-                         (l:84.33, t:884.12, w:239.10, h:12.24)
// (99.9999%) and Ge (99.9999%). They were mixed in the                         (l:347.35, t:887.28, w:239.11, h:8.97)
// lated by Pugacheva and Jezierski [3]. Their results show                     (l:84.33, t:896.08, w:239.10, h:8.97)
// desired proportion and sealed in evacuated silica tubes.                     (l:347.35, t:899.24, w:239.10, h:8.97)
// that mostly magnetic moments are localised on the Mn atom                    (l:84.33, t:908.03, w:239.11, h:8.97)
// To prepare Rh_2_MnSn, the mixture of Rh, Mn and Sn was                       (l:347.35, t:911.19, w:239.10, h:9.94)
// and depend on the local atomic order in the alloys. The                      (l:84.33, t:919.99, w:239.10, h:8.97)
// heated at 250 ^◦^C for 7 h, annealed at 700 ^◦^C for 6 days and              (l:347.35, t:922.89, w:239.10, h:9.22)
// magnetic moment on the Rh atom is small (approximately                       (l:84.33, t:931.94, w:239.10, h:8.97)
// quenched in water. The reaction products were pulverized,                    (l:347.35, t:935.10, w:239.11, h:8.97)
// 0.4µ _B_/atom) and the magnetic properties of these alloys are               (l:84.33, t:943.12, w:239.10, h:10.72)
// mixed, heated again in evacuated silica tubes at 700 ^◦^C for                (l:347.35, t:946.80, w:239.10, h:9.22)
// mainly connected with those of the Mn atoms. The relation-                   (l:84.33, t:955.85, w:239.10, h:8.97)
// 6 days, and then quenched in water. To prepare Rh_2_MnGe,                    (l:347.35, t:959.01, w:239.10, h:9.94)
// ship between the Mn–Mn interatomic distance and the Curie                    (l:84.33, t:967.81, w:239.10, h:8.97)
// the mixture of Rh, Mn and Ge was annealed at 950 ^◦^C for                    (l:347.35, t:970.71, w:239.10, h:9.22)
// temperature has been studied for L2_1_-type and C1_b_-type                   (l:84.33, t:979.76, w:239.10, h:9.98)
// 6 days and quenched in water. The reaction products were                     (l:347.35, t:982.92, w:239.10, h:8.97)
// Heusler alloys [4]. The results show that the Curie tempera-                 (l:84.33, t:991.72, w:239.11, h:8.97)
// pulverized, mixed, heated again in evacuated silica tubes at                 (l:347.35, t:994.88, w:239.11, h:8.97)
// ture of both alloys depends on the Mn–Mn interatomic dis-                    (l:84.33, t:1003.67, w:239.10, h:8.97)
// 950 ^◦^C for 6 days, and then quenched in water.                             (l:347.35, t:1006.58, w:190.23, h:9.22)
// tance and the number of valence electrons.                                   (l:84.33, t:1015.63, w:172.88, h:8.97)
// The powder X-ray diffraction measurements were per-                          (l:357.31, t:1018.79, w:229.14, h:8.97)
// In this paper we report on the pressure dependence of                        (l:94.30, t:1027.58, w:229.14, h:8.97)
// formed with Cu K radiation at room temperature. The                          (l:347.35, t:1030.74, w:239.10, h:8.97)
// the Curie temperature and discuss the interatomic distance                   (l:84.33, t:1039.54, w:239.11, h:8.97)
// obtained diffraction patterns indicated that the prepared                    (l:347.35, t:1042.70, w:239.10, h:8.97)
// samples were single phase with the ordered L2_1_-type struc-                 (l:347.35, t:1054.65, w:239.10, h:9.94)
// ture. The lattice parameters a of Rh_2_MnSn and Rh_2_MnGe                    (l:347.35, t:1066.61, w:239.10, h:9.94)
// ∗ Corresponding author. Fax: _+_81-238-26-3381.                              (l:93.67, t:1069.79, w:158.92, h:9.96)
// are determined to be 6.24 and 6.03 Å, respectively.                          (l:347.35, t:1078.56, w:205.25, h:8.97)
// E-mail address: adachy@yz.yamagata-u.ac.jp (Y. Adachi).                      (l:93.90, t:1079.93, w:191.63, h:7.17)
// 0925-8388/$ – see front matter © 2004 Elsevier B.V. All rights reserved.     (l:84.33, t:1102.84, w:242.79, h:7.17)
// doi:10.1016/j.jallcom.2004.04.003                                            (l:84.33, t:1112.80, w:110.48, h:7.17)
