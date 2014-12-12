package resolver.parser.document

import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations.{TextAnnotation, TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.{ModCollinsHeadFinder, HeadFinder}
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.util.{CoreMap, PropertiesUtils}

/**
 * Created by dimberman on 11/9/14.
 *
 *
 */

object SentenceToMentionConverter {
  var referenceID = 0


  def extract(doc: ConLLSentenceContainer): List[FeatureSet] = {
    referenceID = 0
    doc.sentenceList.foldLeft(List[FeatureSet]())((a: List[FeatureSet], c: CoNLLSentence) =>  a ++ extractFeatures(c, doc))
  }

  val extractFeatures: (CoNLLSentence, ConLLSentenceContainer) => List[FeatureSet] = (s: CoNLLSentence, d: ConLLSentenceContainer) => {
    s.corefs.foldLeft(List[FeatureSet]())((l: List[FeatureSet], c: Coref) => l :+ (anaylzeCoref(c, s, d)))
  }

  val anaylzeCoref: (Coref, CoNLLSentence, ConLLSentenceContainer) => FeatureSet = (c: Coref, s: CoNLLSentence, d: ConLLSentenceContainer) => {

    val corefPhrase = pullPhrase(c, s.words)
    println("coref phrase: " + corefPhrase)
    println(s.words)
    println(s.words.length)
    var prevWord = ""
    var prevType = ""

    if (c.start == 0) {
      if (s.sentenceNum == 0) {
        prevWord = ""
        prevType = ""
      }
      else {
        val previousSentence = d.getSentence(s.sentenceNum - 1)
        prevWord = previousSentence.words.init.last
        prevType = previousSentence.wordType.init.last
      }

    }
    else {
      prevWord = s.getWord(c.start - 1).text
      prevType = s.wordType(c.start - 1)

    }

    var nextWord = ""
    var nextType = ""
    println(s.sentenceNum+" "+d.sentenceList.length)
    if (s.sentenceNum < d.sentenceList.length-1) {
      println(s.sentenceNum+" "+d.sentenceList.length)
      val nextSentence = d.getSentence(s.sentenceNum + 1)
      nextWord = if (c.end == s.length - 1) d.getSentence(s.sentenceNum + 1).firstWord() else s.getWord(c.end + 1).text
      nextType = if (c.end == s.length - 1) nextSentence.wordType(0) else s.wordType(c.end + 1)
    }
    else{
      nextType=""
      nextWord=""
    }
    val POS = new POSHolder(prevType, s.wordType(c.start), s.wordType(c.end), nextType)
    val firstWord = s.words(c.start)
    val lastWord = s.words(c.end)
    val refID = referenceID
    referenceID += 1
    val mentionID = c.corefID
    new FeatureSet(mentionID, refID, s.sentenceNum, "insert type", corefPhrase, "insert head", firstWord, lastWord, prevWord, nextWord, POS)


  }


  def pullPhrase(c: Coref, words: List[String]): String = {
    val nums = wordNums(words.length, List[Int]())
    val relevent = nums.zip(words).filter((a: (Int, String)) => a._1 >= c.start && a._1 <= c.end).foldLeft("")((s: String, a: (Int, String)) => s + a._2 + " ")
    if (relevent.length == 0)
      println("relevent words " + words + " " + c.start + " " + c.end)
    relevent.substring(0, relevent.length - 1)
  }

  def sentenceString(a: List[String]): String = {
    a.foldLeft("")((s: String, b: String) => s + b)
  }

  def wordNums(i: Int, l: List[Int]): List[Int] = {
    if (i == -1) l.reverse else wordNums(i - 1, l :+ i)
  }


  def corefExistsInSentence(s: CoNLLSentence, c: Coref): Boolean = {
    if (s.isEmpty)
      return false
    else s.head().hasCoref(c) || corefExistsInSentence(s.tail(), c)
  }

  def findHeadWord(s: String): String = {
    val prop = new Properties()
    prop.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    val a = new StanfordCoreNLP(prop)
    val doc = new Annotation(s)
        a.annotate(doc)
        val sentence:CoreMap = doc.get(classOf[SentencesAnnotation]).get(0)
        val tree = sentence.get(classOf[TreeAnnotation])
        println("annotated sentence: " + sentence.toString)
        println("annotated tree: " + tree.toString)
        val headFinder:HeadFinder = new ModCollinsHeadFinder


        headFinder.determineHead(tree).toString
    //
//    ""

  }


}

class POSHolder(val prev: String, val first: String, val last: String, val next: String)

class FeatureSet(mID: Int, rID: Int, sNum: Int, mType: String, cString: String, sHead: String, fWord: String, lWord: String, pWord: String, nWord: String, pos: POSHolder) {
  val mentionID: Int = mID
  val refID: Int = rID
  val sentenceNum: Int = sNum
  val mentionType: String = mType
  val completeString: String = cString
  val partsOfSpeech: POSHolder = pos
  val semanticHead: String = sHead
  val firstWord: String = fWord
  val lastWord: String = lWord
  val previousWord: String = pWord
  val nextWord: String = nWord
  var predictedMentionID = -1
}
