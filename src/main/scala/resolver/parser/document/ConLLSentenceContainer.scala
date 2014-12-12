package resolver.parser.document
import edu.stanford.nlp.trees.Tree

/**
 *
 * Created by dimberman on 11/28/14.
 *
 * Sentences: a list of a list of words (each list being a single sentence). Used for Prev word, Next word
 * corefs: for every word, have a list of the coref chains
 * isHead: whether this word is a head word
 *
 */
class ConLLSentenceContainer(id:String, doc:Seq[CoNLLSentence]){

      val sentenceList = doc
      def getSentence(i:Int): CoNLLSentence ={
          doc(i)
      }
}


class CoNLLSentence(sentence:(List[String], List[Coref], List[Boolean], List[String], Tree, Int)){
  val words = sentence._1
  val corefs = sentence._2
  val isHead = sentence._3
  val wordType = sentence._4
  val tree = sentence._5
  val length = words.length
  val sentenceNum = sentence._6


  def isEmpty():Boolean = {
    words.isEmpty
  }

  def tail():CoNLLSentence = {
    new CoNLLSentence(words.tail, corefs.tail, isHead.tail, wordType.tail, tree, sentenceNum)
  }

  def head():Word = {
      this.getWord(0)
  }

  def getWord(i:Int):Word ={
    new Word(words(i), wordType(i))
  }
  def lastWord() = words.last
  def firstWord() = words.head
}


class Word(t:String, wT:String){
  val text = t
   val wordType = wT
  def hasCoref(c:Coref) = {
     false
  }
}


class Coref(st:Int, cfEnd:Int, c:Int){
   val start = st
   val end = cfEnd
  val corefID = c
  var predictedCorefID = -1
  def equals(other:Coref)={
     if(other.start==start && other.end == end && corefID == corefID) true else false
  }
}