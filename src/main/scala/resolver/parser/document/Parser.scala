package resolver.parser.document

import java.io.File

/**
 * Created by dimberman on 12/11/14.
 */
object Parser {
     def parse(file:File):Document = {
       println("Parse begin: "+file.getName)
       val sentences = CoNLLParser.parse(file)
       val features = SentenceToMentionConverter.extract(sentences)
       println("Finished parsing document name: "+file.getName)
       new Document(features.toArray)
     }
}
