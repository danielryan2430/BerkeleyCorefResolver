package classifier

import resolver.parser.document._
import scala.util.Random

/**
 * Created by serenity on 12/3/14.
 */
object adaGradTrainer {
  def train(documents:Seq[Document], eta:Double, lambda:Double, iterMax:Int, featureCount:Int, featureExtractor: (Document, Int, Int) => Seq[Double], lossFunction:(Document, Int, Int) => Double):Seq[Double] = {
    var grad =  List.fill(featureCount)(0.0)
    var dgrad = List.fill(featureCount)(0.0)
    var weights =     List.fill(featureCount)(0.0)
    var error=0.0
    var iter = 0
    while( iter<iterMax & error>.0001) {
      grad =  List.fill(featureCount)(0.0)
      dgrad = List.fill(featureCount)(0.0)
      error=0.0
      for (d <- Random.shuffle(documents)) {

        grad = computeGradient(d,lossFunction,bayesianClassifier.scoreVect(weights,d,featureExtractor), featureExtractor)
        dgrad = dgrad.zip(grad).map { case ((dgtii: Double, gti: Double)) => dgtii + gti * gti}
        val etaOverHtii = dgrad.map(dgtii => eta/(Math.sqrt(dgtii)+1))
        weights =( List[Double]() /: weights.zip(grad).zip(etaOverHtii)) { case (accm, ((xti:Double,gti:Double),eohtii:Double)) => {val inter= xti +gti*eohtii
            accm :+ Math.signum(inter)*(Math.abs(inter) - lambda*eohtii)}
        }
      }
      iter+=1
    }

    weights.toList
  }

  def computeGradient(document:Document,lossFunction:(Document, Int, Int) => Double, scores:Seq[Seq[Double]], featureExtractor: (Document, Int, Int) => Seq[Double]):List[Double] = {
  /*
    A gradient is marginals Gold - marginals predicted. Thus the max is 0 when there is no error amount. If the score is high and the loss is high, the error wil be grown
   */

  }

  def computeMarginals(document:Document, scores:Seq[Seq[Double]], lossFunction:(Document, Int, Int) => Double) : Seq[Seq[Double]] ={
    scores.zipWithIndex.map{case ((scoresi:Seq[Double], mention:Int)) => {
      var marginalOveri =0.0
      scoresi.zipWithIndex.map{case ((scoreij:Double, antecedent:Int))=> {
        val unregMij=Math.exp(scoreij+lossFunction(document,mention,antecedent))
        marginalOveri+=unregMij
        unregMij}}.map(unregularized=> unregularized/marginalOveri)}}
  }

  def computeGoldMarginals(document:Document, scores:Seq[Seq[Double]], lossFunction:(Document, Int, Int) => Double) : Seq[Seq[Double]] ={
    scores.zipWithIndex.map{case ((scoresi:Seq[Double], i:Int)) => {var marginalOveri =0.0
      scoresi.zipWithIndex.map{case ((scoreij:Double, j:Int))=> {val unregMij= if(document.isGold(i,j)) Math.exp(scoreij+lossFunction(document,i,j)) else 0
        marginalOveri+=unregMij
        unregMij}}.map(unregularized=> unregularized/marginalOveri)}}
  }

  def updateGradient(gradient:List[Double], features:Seq[Double], weight:Double): List[Double] = {
    gradient.zip(features).map{case ((gk:Double,fk:Double)) => if (fk >= 1.0e-20) gk+weight else gk}
  }
}



