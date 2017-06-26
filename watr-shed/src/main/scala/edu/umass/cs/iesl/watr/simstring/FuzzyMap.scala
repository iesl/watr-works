package edu.umass.cs.iesl.watr
package simstring

class FuzzyMap[T](tuples:List[(String, T)]) extends Traversable[(String,T)]{

    private val hashMap = tuples.toMap
    private val simString =  SimString(tuples.map(_._1))

    private def search(query:String, threshold:Double,  measure:Measure)={
        simString.search(query, threshold, measure).toList
    }

    def getMatches(query:String, threshold:Double,  measure:Measure): Option[List[(String, T)]] ={

        val setFuzzyMatches = search(query, threshold, measure)
        val pairs = setFuzzyMatches.map{
            matchedString => (matchedString, hashMap.get(matchedString).get )
        }

        if (pairs.size == 0)
            None
        else
            Some(pairs)

    }

    def get(query:String, threshold:Double,  measure:Measure): Option[List[T]] ={

        val setFuzzyMatches = search(query, threshold, measure)
        val values = setFuzzyMatches.flatMap(hashMap.get(_))

        if (values.size == 0)
            None
        else
            Some(values)

    }

    def foreach[U](f: ((String, T)) => U) = hashMap.foreach(f)
}

object FuzzyMap{
    def apply[T](tuples:List[(String, T)]) = new FuzzyMap(tuples)
}