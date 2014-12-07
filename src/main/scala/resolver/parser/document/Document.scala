package resolver.parser.document

/**
 * Created by dimberman on 12/7/14.
 */
class Document(document:Array[FeatureSet]){
      def isGold(mention:Int, antecedent:Int): Boolean ={
         document(mention).mentionID==document(antecedent).mentionID
      }
}
