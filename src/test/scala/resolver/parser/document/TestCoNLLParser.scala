package resolver.parser.document

import org.scalatest.{Matchers, FunSuite}
import Matchers._

/**
 * Created by dimberman on 12/4/14.
 */
class TestCoNLLParser extends FunSuite {
  val parser =  CoNLLParser

  test("test pullNum") {
    assert(parser.pullOutInt("135", "")._1 == 135)
  }
  test("testing pullNum with postvalues") {
    assert(parser.pullOutInt("135)", "")._1 == 135)
  }

  test("pullNum returns -1 if first value not a num") {
    assert(parser.pullOutInt("(135", "")._1 === -1, "parser actually returned: " + parser.pullOutInt("(135", "")._1 === -1)
  }

  test("*********coref parsing*******************") {}
  val corefString1 = "(5|(35|(14)|(3"
  val corefString2 = "5|3|35)"
  val parsedStarts = parser.parseCoref(corefString1, List[(Int, String)]())
  val parsedEnds = parser.parseCoref(corefString2, List[(Int, String)]())
  test("can parse coref beginnings") {
    assert(parsedStarts(0) ==(5, "start"))
  }
  test("can parse coref beginnings in middle of statement") {
    assert(parsedStarts(1) ==(35, "start"))
  }

  test("can parse start in contained statement") {
    assert(parsedStarts(2) ==(14, "start"))
  }

  test("can parse end in contained statement") {
    assert(parsedStarts(3) ==(14, "end"))
  }

  test("can parse coref at end of statement") {
    assert(parsedStarts(4) ==(3, "start"))
  }



  test("can parse coref ends") {
    assert(parsedEnds(0) ==(5, "end"))
  }
  test("can parse coref ends in middle of statement") {
    assert(parsedEnds(2) ==(35, "end"))
  }
  test("can parse coref end at end of statement") {
    assert(parsedEnds(1) ==(3, "end"))

  }
  test("************coref creation**************") {}
  val cf: List[List[(Int, String)]] = List[List[(Int, String)]](parsedStarts, parsedEnds)

  val starts = parser.findCorefStarts(cf.head, List[(Int, String)]())
  test("starts returns 3 starts") {
    assert(starts.length == 4)
  }
  test("starts returns in correct order") {
    assert(starts(0)._1 == 5)
    assert(starts(1)._1 == 35)
    assert(starts(2)._1 == 14)
    assert(starts(3)._1 == 3)
  }


  val distances = parser.findCorefEndDistance(starts, cf, 0)

  test("finds correct distance for different indexed ending") {
    assert(distances(0) == 1)
  }

  test("finds correct distance for self contained corefs") {
    assert(distances(2) == 0)
  }

  val coref = parser.createCorefs(cf, List[Coref](), 0)
  test("created correct corefs") {
    assert(coref(0).equals(new Coref(0, 1, 5)))
    assert(coref(1).equals(new Coref(0, 1, 35)))
    assert(coref(2).equals(new Coref(0, 0, 14)))
    assert(coref(3).equals(new Coref(0, 1, 3)))
  }


}
