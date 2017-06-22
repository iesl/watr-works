package edu.umass.cs.iesl.watr
package simstring

abstract class Measure {
    def t(threshold:Double, sizeOfA:Int, sizeOfB:Int):Int
    def min(threshold:Double, sizeOfA:Int):Int
    def max(threshold:Double, sizeOfA:Int):Int
}

object Cosine extends Measure{

    override def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int):Int={
        Math.ceil(threshold * Math.sqrt(sizeOfFeaturesA * sizeOfFeaturesB)).toInt
    }

    override def max(threshold:Double, sizeOfFeaturesA:Int):Int={
        Math.floor(sizeOfFeaturesA/(threshold * threshold)).toInt
    }

    override def min(threshold:Double, sizeOfFeaturesA:Int):Int={
        Math.ceil(threshold * threshold * sizeOfFeaturesA).toInt
    }


}

object Jaccard extends Measure{


    override def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int):Int={
        Math.ceil( (threshold*(sizeOfFeaturesA + sizeOfFeaturesB)) / (1 + threshold)).toInt
    }

    override def max(threshold:Double, sizeOfFeaturesA:Int):Int={
        Math.floor(sizeOfFeaturesA/threshold).toInt
    }

    override def min(threshold:Double, sizeOfFeaturesA:Int):Int={
        Math.ceil(threshold * sizeOfFeaturesA).toInt
    }
}

object Dice extends Measure{


    override def t(threshold:Double, sizeOfFeaturesA:Int, sizeOfFeaturesB:Int):Int={
        Math.ceil( 0.5 *  threshold * (sizeOfFeaturesA + sizeOfFeaturesB) ).toInt
    }

    override def max(threshold:Double, sizeOfFeaturesA:Int):Int={
        Math.floor( ((2-threshold)/threshold) * sizeOfFeaturesA ).toInt
    }

    override def min(threshold:Double, sizeOfFeaturesA:Int):Int={
        Math.ceil( (threshold/(2-threshold)) * sizeOfFeaturesA ).toInt
    }
}