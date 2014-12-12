package resolver.parser.document

import edu.stanford.nlp.trees.{ModCollinsHeadFinder, Tree}
import org.scalatest.FunSuite

/**
 * Created by dimberman on 12/6/14.
 */
class TestSentenceToMentionConverter extends FunSuite{
    val sExtractor =  SentenceToMentionConverter
              test("nonempty"){
                assert(1==1)
              }
    val phrase = List[String]("I", "sure", "am", "glad", "that",  "I'm", "not", "Bill", "Clinton's", "Intern")
    val coref = new Coref(7,8,5)
    test("pull phrase correctly pulls a phrase"){
      assert(sExtractor.pullPhrase(coref, phrase)=="Bill Clinton's")
    }

    test("pull head from premade tree"){
      val s = "(S (NP (NNP Billy)) (VP (VBD walked) (PP (TO to) (NP (DT the) (NN park)))))"
      val t:Tree = Tree.valueOf(s)
      val h = new ModCollinsHeadFinder
      assert(h.determineHead(t).firstChild().toString=="(VBD walked)")
    }



//    test("headFinder finds head of phrase"){
//      assert(sExtractor.findHeadWord("Billy walked to the park")=="Billy")
//    }

}
