package resolver.parser.document

import org.scalatest.FunSuite

/**
 * Created by dimberman on 12/6/14.
 */
class TestSentenceToMentionConverter extends FunSuite{
    val sExtractor = new SentenceToMentionConverter
              test("nonempty"){
                assert(1==1)
              }
    val phrase = List[String]("I", "sure", "am", "glad", "that",  "I'm", "not", "Bill", "Clinton's", "Intern")
    val coref = new Coref(7,8,5)
    test("pull phrase correctly pulls a phrase"){
      assert(sExtractor.pullPhrase(coref, phrase)=="Bill Clinton's")
    }

    test("headFinder finds head of phrase"){
      assert(sExtractor.findHeadWord("Billy walked to the park")=="billy")
    }

}
