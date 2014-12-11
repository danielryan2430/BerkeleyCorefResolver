package classifier

import resolver.parser.document
import org.scalatest.{Matchers, FunSuite}
import Matchers._
/**
 * Created by serenity on 12/10/14.
 */
class TestLossFunction extends FunSuite
  val f1 = FeatureSet(10, 2, 3, "", "", "", "", "", "", "")
  val f2
  val  doc =
  val lossFn = adaGradTrainer.lossFucntionGen(1,2,3)
  test("fn") {
    assert(lossFn())
  }
  test("wl") {
  assert(parser.pullOutInt("135", "")._1 == 135)
  }
  test("fa") {
  assert(parser.pullOutInt("135", "")._1 == 135)
  }
  test("correct") {

  }
}
