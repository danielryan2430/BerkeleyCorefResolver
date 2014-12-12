package classifier

import resolver.parser.document._

/**
 * Created by serenity on 11/10/14.
 */
object bayesianClassifier {

  def classify(weights: Seq[Double], d: Document, featureExtractor: (Document, Int, Int) => Seq[Int]): List[Int] = {
    //using weight vector, create a score for a featureSet
    def weightMult: Seq[Int] => Double = (input: Seq[Int]) => input.zip(weights).foldLeft(0.0) { case (a, (b, c)) => a + b * c}




   def findBestCorrelatedFeature(maxChoiceAccum: (Int, Double), maxChoiceFeat: FeatureSet, liveFeature: FeatureSet): (Int, Double) = {
      //1. the returned feature has a base setting of new (score btw f.ID and itself...)
      val C = weightMult(featureExtractor(d, liveFeature.refID, maxChoiceFeat.refID))
      if (C > maxChoiceAccum._2) {
        (maxChoiceFeat.refID, C)
      } // if You have a higher score, choose you
      else {
        maxChoiceAccum
      } //other
    }




    //D:(List[Int] = mentionIDs(A), List[FeatureSet] = previous mention features), foldLeft all feature descriptions in Document D
    ((List[Int](), List[FeatureSet]()) /: d.features) {
      //Part one of the accum  the second part of the accumulator holds all of the references seen so far so we can build the mention list up correctly.
      //D: accum is the current list of tuples(refID, FeatureSet) while liveFeature represents a current mention that is being used to generate a new tuple value
      (accum, liveFeature) => (accum._1 :+ ((liveFeature.refID, /*1*/ weightMult(featureExtractor(d, liveFeature.refID, liveFeature.refID))) /: accum._2)
            {(maxChoiceAccum, maxChoiceFeat) => findBestCorrelatedFeature(maxChoiceAccum, maxChoiceFeat, liveFeature)}._1 // then return the choice Made.
      , accum._2 :+ liveFeature) // bind to it the list of all choices to consider next time
    }._1 //return the list of assigned mentions--it is ordered so it directly corresponds to an A vector.
  } // finish up.


  //does the same thing as classification, except instead of returning the maximum, it simply returns the entire list of score vectors. (so doc 1 has 1, doc 2 has 2...
  def scoreVect(weights: Array[Double], d: Document, featureExtractor: (Document, Int, Int) => Seq[Int]):Seq[Seq[Double]] = {
    //using weight vector, create a score for a featureSet
    def weightMult: Seq[Int] => Double = (input: Seq[Int]) => {
      input.foldLeft(0.0) { case (accm, inputInd) => if(inputInd> -1)accm + weights(inputInd) else accm}
    }
    def scoreFeatures(scoreList: List[Double], newFeat: FeatureSet, liveFeature: FeatureSet): List[Double] = {
      //1. the returned feature has a base setting of new (score btw f.ID and itself...)
      val C = weightMult(featureExtractor(d, liveFeature.refID, newFeat.refID))
      scoreList :+ C
    }


    //D:(List[ListDouble] = scores, List[FeatureSet] = previous mention features), foldLeft all feature descriptions in Document D
    ((List[List[Double]](), List[FeatureSet]()) /: d.features) {
      //Part one of the accum  the second part of the accumulator holds all of the references seen so far so we can build the mention list up correctly.
      //D: accum is the current list of tuples(refID, FeatureSet) while liveFeature represents a current mention that is being used to generate a new tuple value
      (accum:(List[List[Double]],List[FeatureSet]), liveFeature:FeatureSet) => (accum._1 :+ ((List(weightMult(featureExtractor(d, liveFeature.refID, liveFeature.refID))) /: accum._2)
        {(scoreList:List[Double], newFeat:FeatureSet) => scoreFeatures(scoreList, newFeat, liveFeature)}.reverse) // then return the choice Made.
        , liveFeature +: accum._2 ) // bind to it the list of all choices to consider next time backwards. Now the score matrix is entirly backwards so reverse it.
    }._1 //return the list of assigned mentions--it is ordered so it directly corresponds to an A vector.
  }
}
