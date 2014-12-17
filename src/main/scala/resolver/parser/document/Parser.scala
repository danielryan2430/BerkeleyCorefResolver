package resolver.parser.document

import java.io.File

import scala.collection.mutable.ListBuffer


object Parser {
     def parse(file:File):List[Document] = {
       println("Parse begin: "+file.getName)
       val sentences = CoNLLParser.parse(file)
       println("parsed the files, now we will convert to mentions")
       val hold = new ListBuffer[Document]
       for(sentence <- sentences) {
         val features = SentenceToMentionConverter.extract(sentence)
         hold+=new Document(features.toArray, sentence.id)
       }
       println("Finished parsing document name: "+file.getName)
       hold.toList
     }
}
