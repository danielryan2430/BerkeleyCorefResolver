package resolver.parser.document

import java.io.File

import edu.stanford.nlp.trees.Tree

import scala.collection.mutable


/**
 * Created by dimberman on 11/28/14.
 */
object CoNLLParser {
  var sentenceNum = 0

  def parse(fileName: File): List[ConLLSentenceContainer] = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines()
    var firstLine = lines.next()
    val hold = new mutable.MutableList[ConLLSentenceContainer]
    while(!lines.isEmpty)
    {
      println("doc name pulled: " + firstLine)
      //parse firstline to get docID
      val s = new ConLLSentenceContainer(firstLine, parseByLine(lines, Seq[CoNLLSentence](),
        List[String](),
        List[List[(Int, String)]](),
        List[String](),
        List[String](),
        List[String]()))
      if(!lines.isEmpty)
        firstLine=lines.next()
      println("num sentences in section: " +s.sentenceList.length)
      hold:+s

    }
    hold.toList
  }




  def parseByLine(restOfDoc: Iterator[String],
                  acc: Seq[CoNLLSentence],
                  currSentence: List[String],
                  currCorefs: List[List[(Int, String)]],
                  currIsHead: List[String],
                  currType: List[String],
                  treeSegs: List[String])
  : Seq[CoNLLSentence] = {
//    println("cutting")
    val cutLine = restOfDoc.next.split("\\s+")
    if (cutLine(0)=="#end"){
      println("finished parsing to sentences")
      return acc
    }
    //gives current line and moves iterator

    if(cutLine.length<5){
      return parseByLine(restOfDoc, acc, currSentence, currCorefs, currIsHead, currType, treeSegs)

    }
//    println("cutline elements " + cutLine.toString)
    val coref = parseCoref(cutLine.last, List[(Int, String)]())
//    println("coref parsed")
    val word = cutLine(3)
    val wordType = cutLine(4)
    val treePiece = cutLine(5)
    lexicalCounter.addWord(word)
    if (word == "/."|| word == "/?" || word =="/!") {
      val t: Tree = makeTree(treeSegs:+treePiece)
      val add = new CoNLLSentence(currSentence:+word, createCorefs(currCorefs:+coref, List[Coref](), 0), isHead(currIsHead:+wordType, t), currType:+wordType, t, sentenceNum)
      sentenceNum += 1

      println("adding sentence: " +sentenceString(add.words))

      return parseByLine(restOfDoc, acc :+ add, List[String](), List[List[(Int, String)]](), List[String](), List[String](), List[String]())
    }
    else {
//      println("adding word" + word)
      return parseByLine(restOfDoc, acc, currSentence :+ word, currCorefs :+ coref, currIsHead :+ wordType, currType :+ wordType, treeSegs :+ treePiece)
    }
  }

  def sentenceString(a: List[String]): String = {
    a.foldLeft("")((s: String, b: String) => s + " " + b)
  }


  def parseCoref(s: String, a: List[(Int, String)]): List[(Int, String)] = {
    var currentNum = ""
    if (s.isEmpty||s.head=='-') a
    else if (s.head == '(') {
      val hold = pullOutInt(s.tail, "")
      if(!hold._2.isEmpty && hold._2.head==')') parseCoref(hold._2, a++List[(Int,String)]((hold._1, "start"),(hold._1,"end")))
      else parseCoref(hold._2, a :+(hold._1, "start"))
    }
    else{
      if (s.head == ')' || s.head == '|') parseCoref(s.tail, a)
      else {
        val hold = pullOutInt(s, "")
        parseCoref(hold._2, a :+(hold._1, "end"))
      }
    }
  }


  def pullOutInt(s: String, i: String): (Int, String) = {
    if(s.isEmpty || !s.head.isDigit){
      if (i.isEmpty) (-1,s)
      else (i.toInt, s)
    }
    else {
      pullOutInt(s.tail, i :+ s.head)
    }
  }


  def createCorefs(s: List[List[(Int, String)]], cf: List[Coref], index: Int): List[Coref] = {
    if (s.isEmpty) cf
    else {
      val starts = findCorefStarts(s.head, List[(Int, String)]())
      val ends = findCorefEndDistance(starts, s,0)
      val corefs = completeCorefs(starts, ends, index)
      createCorefs(s.tail, cf ++ corefs, index + 1)
    }
  }

  def findCorefStarts(s: List[(Int, String)], a: List[(Int, String)]): List[(Int,String)] = {
    if (s.isEmpty) a
    else if (s.head._2 == "start"){
//      print("start found: " + s.head._1)
      findCorefStarts(s.tail, a :+ s.head)
    }
    else findCorefStarts(s.tail, a)
  }
  
  def findCorefEndDistance(starts:List[(Int,String)],rest:List[List[(Int, String)]], distance:Int ): List[Int] ={
        starts.foldLeft(List[Int]())((a:List[Int], b:(Int,String))=>a:+findEndForStart(b, rest, 0))
  }


  def findEndForStart(start:(Int,String),rest:List[List[(Int, String)]], distance:Int ): Int = {
        if(rest.isEmpty) return -1
        val currentSpot = rest(0)
        val hasEnding = currentSpot.filter((a:(Int,String))=>a._1==start._1 && a._2=="end")
        if(hasEnding.isEmpty)findEndForStart(start, rest.tail, distance+1)
        else distance
  }

  def completeCorefs(starts: List[(Int, String)], endDistances:List[Int], ind: Int): List[Coref] = {
        starts.zip(endDistances).foldLeft(List[Coref]())((a:List[Coref], b:((Int,String),Int))=> a:+ new Coref(ind, ind+b._2, b._1._1))
  }


  def makeTree(s: List[String]): Tree = {
    //TODO: do we went a tree?
    val tmp = s.mkString("")
    return Tree.valueOf(tmp)
//    Tree.valueOf("()")
  }

  //Takes in the sentence tree and returns a list of booleans deciding whether each word
  //is a boolean or not
  def isHead(s: List[String], t: Tree): List[Boolean] = {
    List[Boolean](false)
  }
}


object lexicalCounter{
  val wordCountMap = scala.collection.mutable.Map[String, Int]()
  def addWord(s:String): Unit ={
      wordCountMap.get(s)match{
      case Some(i) =>    wordCountMap.update(s,i+1)
      case None => wordCountMap += s -> 1
    }
  }

}


