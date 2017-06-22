package edu.umass.cs.iesl.watr
package simstring

object NGram{

    def apply(string:String)={
        val trigrams = ("££" + string + "££").toCharArray().sliding(3)
        trigrams.map(_.mkString("")).toList
    }

}
