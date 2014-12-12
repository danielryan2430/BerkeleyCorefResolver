package resolver.parser.document

import java.io.File



/**
 * Created by dimberman on 12/11/14.
 */
object Parser {
     def parse(file:File):List[Document] = {
       println("Parse begin: "+file.getName)
       val sentences = CoNLLParser.parse(file)
       val hold = new collection.mutable.MutableList[Document]
       for(sentence <- sentences) {
         val features = SentenceToMentionConverter.extract(sentence)
         hold:+new Document(features.toArray)

       }
       println("Finished parsing document name: "+file.getName)
       hold.toList
     }
}
