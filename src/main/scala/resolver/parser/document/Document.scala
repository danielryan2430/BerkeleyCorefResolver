package resolver.parser.document

/**
 * Created by dimberman on 12/7/14.
 */
class Document(val features:Array[FeatureSet], val id:String){
      def isGold(mention:Int, antecedent:Int): Boolean ={
         features(mention).mentionID==features(antecedent).mentionID
      }
}
