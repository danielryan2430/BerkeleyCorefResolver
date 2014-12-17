package classifier

import resolver.parser.document._

import scala.util.Random

object adaGradTrainer {
   var round = -1;
  //assumes the antecednt is before the mention.
  def lossFunctionGen(fn:Double,fa:Double,wl:Double) : ((Document,Int,Int)=>Double) = {
    (d:Document,mention:Int,antecedent:Int) => {
      val clust = d.features.zipWithIndex.filter((p:(FeatureSet,Int))=> p._1.mentionID ==d.features(mention).mentionID).map(a=>a._2)

      if(mention == clust(0) && mention != antecedent) fa //false anaphor is when mention should be the first instance of that mention id....
      else if (d.features(mention).mentionID != d.features(antecedent).mentionID) wl // wronglink if the mention ids do not match.
      else if (mention != clust(0) && mention == antecedent) fn //false new if mention is not the first instance of that id, but mention == antecedent
      else 0.0
    }
  }

  def train(documents:Seq[Document], eta:Double, lambda:Double, iterMax:Int, featureCount:Int, featureExtractor: (Document, Int, Int) => Seq[Int],lossFunction:(Document, Int, Int) => Double):Array[Double] = {
    println("beginning training using the adagrad training")
    val grad =  Array.fill(featureCount)(0.0)
    var dgrad = Array.fill(featureCount)(0.0)
    val weights =     Array.fill(featureCount)(0.0)
    val error=Double.PositiveInfinity
    var iter = 0
    while( iter<iterMax & error>.0001) {
      println("Training round: "+iter)
      round += 1
      for(i<-grad.indices)
        dgrad(i)=0.0
      //error=1.0         //not impl currently    TODO:ADD ERROR CHECKING
      for (d <- Random.shuffle(documents)) {
        for(i<-grad.indices)
          grad(i)=0.0
        computeGradient(d,grad,lossFunction,bayesianClassifier.scoreVect(weights,d,featureExtractor), featureExtractor)
        dgrad = dgrad.zip(grad).map { case ((dgtii: Double, gti: Double)) => dgtii + gti * gti}
        val etaOverHtii = dgrad.map(dgtii => eta/(Math.sqrt(dgtii)+1))
        for(i <- weights.indices){
        weights(i) = {
          val inter = weights(i) + grad(i) * etaOverHtii(i)
          Math.signum(inter) * (Math.abs(inter) - lambda * etaOverHtii(i))
        }
        }
         // ( List[Double]() /: weights.zip(grad.toSeq).zip(etaOverHtii)) { case (accm, ((xti:Double,gti:Double),eohtii:Double)) => {val inter= xti +gti*eohtii
          //  accm :+ Math.signum(inter)*(Math.abs(inter) - lambda*eohtii)}
        //}
      }
      iter+=1
    }

    weights.toList.toArray
  }

  def computeGradient(document:Document,gradient:Array[Double],lossFunction:(Document, Int, Int) => Double, scores:Seq[Seq[Double]], featureExtractor: (Document, Int, Int) => Seq[Int]) = {
  /*
    A gradient is marginals Gold - marginals predicted. Thus the max is 0 when there is no error amount. If the score is high and the loss is high, the error wil be grown
   */
    //If classification error increases, so will the marginal scores
    /**
     * this half of the function is performs x^i,A(X) (essentially computing the gradient using the gold mentions so we know the 100% correct direction)
     */

    computeGoldMarginals(document,scores,lossFunction).zipWithIndex.map //take a list of 0's and fold in the indexed marginals
        {case ((margi:Seq[Double], mention:Int)) =>    //for each marginal value
          margi.zipWithIndex.map{case ((margij:Double, antecedent:Int)) =>
          updateGradient(gradient,featureExtractor(document,mention,antecedent),margij)  //update the gradient using the current gradient, the mention, the antecedent, and the current marginal value
      }
    }

    /**
     * perform similar operations, but with the predicted value taking into account the loss function
     */
    computeMarginals(document,scores,lossFunction).zipWithIndex.map{case ((margi:Seq[Double], mention:Int)) =>
            margi.zipWithIndex.map{case ((margij:Double, antecedent:Int)) =>
      updateGradient(gradient,featureExtractor(document,mention,antecedent),-margij)
      }
    }
    println("round "+round + ": "+ "gradient nonzero count: "+(0/:gradient){(a,b)=>if (b>1e-6|| b< -1e-6) a+1 else a})
  }








  def computeMarginals(document:Document, scores:Seq[Seq[Double]], lossFunction:(Document, Int, Int) => Double) : Seq[Seq[Double]] ={
    scores.zipWithIndex.map{case ((scoresi:Seq[Double], mention:Int)) => {
      var marginalOveri =0.0
      scoresi.zipWithIndex.map{case ((scoreij:Double, antecedent:Int))=> {
        val unregMij=Math.exp(scoreij+lossFunction(document,mention,antecedent))
        marginalOveri+=unregMij
        unregMij}
      }.map(unregularized=> unregularized/marginalOveri)}}
  }






  //marginals represent the total marginal likelihood of assigning the correct antecedent using the gold clustering data to determine scoring.
  def computeGoldMarginals(document:Document, scores:Seq[Seq[Double]], lossFunction:(Document, Int, Int) => Double) : Seq[Seq[Double]] ={
    scores.zipWithIndex.map{case ((scoresi:Seq[Double], i:Int)) => {
      var marginalOveri =0.0
      scoresi.zipWithIndex.map{case ((scoreij:Double, j:Int))=> {
        val unregMij= if(document.isGold(i,j)) Math.exp(scoreij+lossFunction(document,i,j)) else 0
        marginalOveri+=unregMij
        unregMij}}.map(unregularized=> unregularized/marginalOveri)}}
  }

  def updateGradient(gradient:Array[Double], features:Seq[Int], weight:Double) = {
    for(ind <- features) {
      if(ind!= -1)
      gradient(ind) += weight
    }
  }
}



