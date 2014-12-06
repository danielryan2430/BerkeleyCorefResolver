package resolver.parser.document


/**
 * Created by dimberman on 11/9/14.
 *
 *
 */

class SurfaceFeatureExtractor {
  var referenceID = 0


  def extract(doc: Document): List[FeatureSet] = {
    doc.sentenceList.foldLeft(List[FeatureSet]())((a: List[FeatureSet], c: CoNLLSentence) => a ++ extractFeatures(c, doc))
  }

  val extractFeatures: (CoNLLSentence, Document) => List[FeatureSet] = (s: CoNLLSentence, d: Document) => {
    s.corefs.foldLeft(List[FeatureSet]())((l: List[FeatureSet], c: Coref) => l :+ (anaylzeCoref(c, s, d)))
  }

  val anaylzeCoref: (Coref, CoNLLSentence, Document) => FeatureSet = (c: Coref, s: CoNLLSentence, d: Document) => {
    val corefPhrase = pullPhrase(c, s.words)
    val prevWord = if (c.start == 0) d.getSentence(s.sentenceNum - 1).lastWord() else s.getWord(c.start - 1).text
    val nextWord = if (c.end == s.length) d.getSentence(s.sentenceNum + 1).firstWord() else s.getWord(c.end + 1).text

    val firstWord = s.words(c.start)
    val lastWord = s.words(c.end)
    val refID = referenceID
    referenceID += 1
    val mentionID = c.corefID


    new FeatureSet(mentionID, refID, s.sentenceNum, "insert type", corefPhrase, "insert head", firstWord, lastWord, prevWord, nextWord)


  }


  def pullPhrase(c: Coref, words: List[String]): String = {
    val nums = wordNums(words.length, List[Int]())
    val relevent = nums.zip(words).filter((a: (Int, String)) => a._1 >= c.start && a._1 <= c.end).foldLeft("")((s: String, a: (Int, String)) => s + a._2+" ")
    relevent.substring(0,relevent.length-1)
  }

  def wordNums(i: Int, l: List[Int]): List[Int] = {
    if (i == -1) l.reverse else wordNums(i - 1, l :+ i)
  }


  def corefExistsInSentence(s: CoNLLSentence, c: Coref): Boolean = {
    if (s.isEmpty)
      return false
    else s.head().hasCoref(c) || corefExistsInSentence(s.tail(), c)
  }


}


class FeatureSet(mID: Int, rID: Int, sNum: Int, mType: String, cString: String, sHead: String, fWord: String, lWord: String, pWord: String, nWord: String) {
  val mentionID: Int = mID
  val refID: Int = rID
  val sentenceNum: Int = sNum
  val mentionType: String = mType
  val completeString: String = cString
  val semanticHead: String = sHead
  val firstWord: String = fWord
  val lastWord: String = lWord
  val previousWord: String = pWord
  val nextWord: String = nWord
}
