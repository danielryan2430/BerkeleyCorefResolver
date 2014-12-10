package resolver.parser.document

/**
 * Created by dimberman on 12/7/14.
 */
class Document(features:Array[FeatureSet]){
      def isGold(mention:Int, antecedent:Int): Boolean ={
         features(mention).mentionID==features(antecedent).mentionID
      }
}
