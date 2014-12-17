package resolver.parser.document

class Document(val features:Array[FeatureSet], val id:String){
      def isGold(mention:Int, antecedent:Int): Boolean ={
        classify(mention,antecedent)==4
      }


  def classify(mention:Int, antecedent:Int) : Int = {
      val clust = features.zipWithIndex.filter((p:(FeatureSet,Int))=> p._1.mentionID ==features(mention).mentionID).map(a=>a._2)
      if (features(mention).mentionID != features(antecedent).mentionID) 1 // wronglink if the mention ids do not match.
      else if(mention == clust(0) && mention != antecedent) 2 //false anaphor is when mention should be the first instance of that mention id....
      else if (mention != clust(0) && mention == antecedent) 3 //false new if mention is not the first instance of that id, but mention == antecedent
      else 4
    }
}
