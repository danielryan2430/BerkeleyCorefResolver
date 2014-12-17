package resolver.parser.document

import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations.{TextAnnotation, TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.{SemanticHeadFinder, Tree, ModCollinsHeadFinder, HeadFinder}
import edu.stanford.nlp.util.{CoreMap, PropertiesUtils}


object SentenceToMentionConverter {
  var referenceID = 0


  def extract(doc: ConLLSentenceContainer): List[FeatureSet] = {
    referenceID = 0
    println("extracting features")

    doc.sentenceList.foldLeft(List[FeatureSet]())((a: List[FeatureSet], c: CoNLLSentence) =>  a ++ extractFeatures(c, doc))
  }

  val extractFeatures: (CoNLLSentence, ConLLSentenceContainer) => List[FeatureSet] = (s: CoNLLSentence, d: ConLLSentenceContainer) => {
    s.corefs.foldLeft(List[FeatureSet]())((l: List[FeatureSet], c: Coref) => l :+ (anaylzeCoref(c, s, d)))
  }

  val anaylzeCoref: (Coref, CoNLLSentence, ConLLSentenceContainer) => FeatureSet = (c: Coref, s: CoNLLSentence, d: ConLLSentenceContainer) => {
//    print("sentence is : "  + sentenceString(s.words) + " in document: " + d.id)
    val corefPhrase = pullPhrase(c, s.words)

//    println("coref phrase: " + corefPhrase)
//    println("document name: " + d.id)
//    println(s.words)
//    println("this is sentence number "+s.sentenceNum)
//    println("it has " + s.words.length + " words")
//    println("according to the document, we should have "+ d.sentenceList.length + " sentences")
    var prevWord = ""
    var prevType = ""

    if (c.start == 0) {
      if (s.sentenceNum == 0) {
        prevWord = ""
        prevType = ""
      }
      else {
        var previousSentence = d.getSentence(s.sentenceNum - 1)
        if(previousSentence.lastWord()!="/."){
          prevWord = previousSentence.words.last
          prevType = previousSentence.partOfSpeech.last
        }
        else{
          while(previousSentence.words.length ==1 && previousSentence.sentenceNum!=0){
            previousSentence=d.getSentence(previousSentence.sentenceNum-1)
          }
          if (s.sentenceNum == 0) {
            prevWord = ""
            prevType = ""
          }
          else{
//            println("previous sentence: " + sentenceString(previousSentence.words))
            prevWord = previousSentence.words.init.last
            prevType = previousSentence.partOfSpeech.init.last
          }
        }
      }

    }
    else {
      if((s.words(c.start-1).head==',' || s.words(c.start-1).head=='\'')&&c.start>1){
        prevWord=s.words(c.start-2)
        prevType = s.partOfSpeech(c.start - 2)

      }
      else{
        prevWord = s.words(c.start - 1)
        prevType = s.partOfSpeech(c.start - 1)
      }

    }

    var nextWord = ""
    var nextType = ""
    if (s.sentenceNum < d.sentenceList.length-1) {
      val nextSentence = d.getSentence(s.sentenceNum + 1)
      if (c.end == s.length - 1){
        nextWord =  d.getSentence(s.sentenceNum + 1).firstWord()
        nextType = nextSentence.partOfSpeech(0)
      }
       else{
        if(s.words(c.end+1).head==',' || s.words(c.end+1).head=='\''){
          nextWord =  s.words(c.end+2)
          nextType = s.words(c.end + 2)
        }
        else {
          nextWord =  s.words(c.end+1)
          nextType = s.words(c.end + 1)
        }
      }
    }
    else{
      nextType=""
      nextWord=""
    }
    val POS = new POSHolder(prevType, s.partOfSpeech(c.start), s.partOfSpeech(c.end), nextType)
    val firstWord = s.words(c.start)
    val lastWord = s.words(c.end)
    val refID = referenceID
    referenceID += 1
    val mentionID = c.corefID
    val headIndex = findHeadIndex(c.start, c.end, c.start, s.headMap)
    new FeatureSet(mentionID, refID, s.sentenceNum, findMentionType(s.partOfSpeech(headIndex), s.treeSegs(headIndex)), corefPhrase, s.words(headIndex), firstWord, lastWord, prevWord, nextWord, POS)


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


  def findMentionType(pos:String, NER:String): String ={
      if(pos == "PRP" || pos == "PRP$") "PRP"
      else if(NER.contains("NNP"))  "NNP"
      else  "NP"
  }
  val headFinder = new ModCollinsHeadFinder()


  def findHeadIndex(start:Int, end:Int, current:Int, head:Map[Int,Int]): Int = {
//    println("finding head index for val: " + current)
    val a = head.get(current)
    a match {
      case Some(b: Int) => {
        if (b == current || b < start || b > end){
//          if(b==start)println("head is same as start")
          current
        }
        else findHeadIndex(start, end, b, head)
      }
      case None => current
    }
  }




  def headWordParser(tree: Tree): String = {
        if(tree==null){
          return ""
        }

        if(tree.isLeaf) return tree.toString
        headFinder.determineHead(tree).toString

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
