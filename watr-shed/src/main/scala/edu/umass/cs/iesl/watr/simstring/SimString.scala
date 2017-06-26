package edu.umass.cs.iesl.watr
package simstring

class SimString(val words:Vector[String]){

    private val wordsToIndex = words.zipWithIndex.toMap

    private val ngramSizeWordTriples = words.map(getFeature(_)).zipWithIndex.flatMap{
        case (ngrams:List[String], wordIndex:Int) =>
            ngrams.map{
                ngram =>
                    (ngram, ngrams.size, wordIndex )
            }
    }

    private val lookupTable = ngramSizeWordTriples.map{
        triple =>
            // ngram_size => wordIndex
            (triple._1 + "_" + triple._2, triple._3)
    }.groupBy(_._1)
        .mapValues(_.map(_._2).toSet)

    private def getFeature(word:String) = NGram(word)

    private def getMatches(size:Int, ngram:String)={
        lookupTable.get(ngram+"_"+size)
    }

    private def overlapJoin(features:List[String], minOverlap:Int, sizeOfQuery:Int)={

        val candidates = features.map(getMatches(sizeOfQuery, _)).flatten.sortBy(_.size)

        /* Given a list counts how many times every item occurs*/
        def countCocurrances(list:List[Int]): Map[Int, Int]={
            list.groupBy(i => i).map(t => (t._1, t._2.length) ).toMap
        }

        val candidatesCounts =  candidates.slice(0, features.size - minOverlap + 1).map(_.toList).flatten
        val narrowedCandidatesSet = candidatesCounts.toSet


        val extraCounts = candidates.slice(features.size - minOverlap + 1, features.size).flatMap{
            currentMatches =>
                narrowedCandidatesSet.toList.flatMap{
                    word =>
                        if (currentMatches.contains(word))
                            Some(word)
                        else
                            None
                }
        }


        val matches = countCocurrances(candidatesCounts ++ extraCounts).filter{ t => minOverlap <= t._2 }.keySet


        matches
    }

    def search(query:String, threshold:Double,  measure:Measure) = {
        val features = getFeature(query)

        val matchesSize = Range(measure.min(threshold, features.size), measure.max(threshold, features.size) + 1)
        val matches = matchesSize.flatMap{
            l =>
                val minOverlap = measure.t(threshold, features.toSet.size, l)
                overlapJoin(features, minOverlap, l)
        }.toSet


        matches.map(this.words(_))
    }

}


object SimString{
    def apply(words:List[String])= new SimString(words.toVector)
}
