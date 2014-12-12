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

    test("paren balancer handles extra left parens"){
      val s  = "(SBAR(S(NP DT NN)"
      assert(sExtractor.balanceParens(s)=="(NP DT NN)")
    }
  test("paren balancer handles extra left parens but with no closes"){
    val s  = "(SBAR(S(NP DT NN"
    assert(sExtractor.balanceParens(s)=="(NP DT NN)")
  }

  test("paren balancer handles extra right parens"){
    val s  = "(NP(NP DT JJ NN)(PP NN (DNP DT NN NN))))))))"
    assert(sExtractor.balanceParens(s)=="(NP(NP DT JJ NN)(PP NN (DNP DT NN NN)))")
  }

  test("penn tree creator creates valid penn tree") {
    val tree =  List[String]("(NP(NP*", "*", "*)", "(PP*", "(NP*", "*", "*))))))))")
    val words = List[String]("the", "fragile", "progress", "toward", "a", "peace", "process")
    assert(sExtractor.createPennTree(words, tree) == "(NP(NP the fragile progress)(PP toward(NP a peace process)))")
  }

  test("headfinder works on one word"){
    assert(sExtractor.headWordParser(Tree.valueOf("(NP the Israeli)"))=="(NP the Israeli)")
  }
  test("head word parser finds head word"){
    val tree =  List[String]("(NP(NP*", "*", "*)", "(PP*", "(NP*", "*", "*))))))))")
    val words = List[String]("the", "fragile", "progress", "toward", "a", "peace", "process")
    assert(sExtractor.findHeadWord(words, tree)=="the fragile progress")
  }


//    test("headFinder finds head of phrase"){
//      assert(sExtractor.findHeadWord("Billy walked to the park")=="Billy")
//    }

}
