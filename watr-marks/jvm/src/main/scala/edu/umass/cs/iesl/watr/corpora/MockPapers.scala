package edu.umass.cs.iesl.watr
package corpora


object MockPapers {
  val numberedMargins = List(
    """|
       |bioRxiv preprint first posted online Jan. 13, 2014 doi: http://dx.doi.org/10.1101/001792
       |
       |1     Human paternal and maternal demographic histories: insights from high-
       |
       |2                resolution Y chromosome and mtDNA sequences
       |
       |3
       |
       |4   Sebastian Lippold1, Hongyang Xu 1,2, Albert Ko1 , Mingkun Li 1,3 , Gabriel Renaud1,
       |
       |5                Anne Butthof ^{1,4} , Roland Schröder 1 , Mark Stoneking 1
       |6
       |7     1 Department of Evolutionary Genetics, Max Planck Institute for Evolutionary
       |
       |8        Anthropology, Leipzig, Germany
       |
       |9     2 Department of Computational Genetics, CAS-MPG Partner Institute for
       |
       |10       Computational Biology, Shanghai, China
       |
       |11    3 Present address: Fondation Mérieux, 17 rue Bourgelat, Lyon, France
       |
       |12    4 Present address: Institute of Biochemist ry, Faculty of Medicine, Unversity of
       |
       |13    Leipzig, Leipzig, Germany
       |14
       |15
       |16            Correspondence should be addressed to M.S. (stoneking@eva.mpg.de)
       |""".stripMargin,
    """|
       | 1   Abstract
       |
       | 2      To investigate in detail the paternal and maternal demographic histories of humans,
       | 3    we obtained ~500 kb of non-recombining Y chromosome (NRY) sequences and
       | 4    complete mtDNA genome sequences from 623 males from 51 populations in the
       |""".stripMargin
  )

  val genericTitle = List(
    """|            The Title of the Paper
       |^{a}Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan
       |""".stripMargin,

    """|   EXPERIMENTAL
       |1. Sample Preparation and Characterization
       |
       |   The starting material of NaBiO_{3} ? nH2O (Nacalai Tesque
       |Inc.) was placed in a Teflon lined autoclave (70 ml) with
       |LiOH and H2O (30 ml) and was heated at 120–2008C
       |for 4 days.
       |
       |""".stripMargin
  )

  def sample_4pg_3x3_doc(): List[String] = {
    List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr",
      "stu\nvwx\nyzz"
    )
  }

}
