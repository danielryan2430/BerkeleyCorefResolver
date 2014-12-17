package classifier


import java.io.{FileWriter, File}

import edu.stanford.nlp.dcoref.Dictionaries
import resolver.parser.document.{PronounDictionary, lexicalCounter, FeatureSet, Document}

class FeatureExtractor(docs:Seq[Document]) {
  val (featIndMap, featCount)=createFeatureIndex(docs)
  var cachedFeatures =Map[(String,Int,Int),Seq[Int]]()
  def extractFeatures(doc: Document, mention: Int, antecedent: Int): Seq[Int] = {
    cachedFeatures.get((doc.id,mention,antecedent)) match{
      case Some(featList) => {
        featList

      }
      case None => {
        val currentMention = doc.features (mention)
        val antMention = doc.features (antecedent)
        val featNames = buildFeatSet (currentMention, antMention)
        val featInd = featNames.map (name => featIndMap.getOrElse (name, {/*println(name);*/ - 1}) )
       cachedFeatures = cachedFeatures +((doc.id,mention,antecedent)-> featInd)
        featInd
      }
    }
  //  println("feature nonzero count: "+(0/:c){(a,b)=>if (b>0) a+1 else a})
  //   c
  }

  def dumpFeatures()={
    val f = new File("./features.txt")
    val write = new FileWriter(f)
    for(pair <- featIndMap)
      write.write(pair.toString()+"\n")
  }

  def featureVal(a: Boolean): Int = if (a) 1 else 0

  //needed feature
  //  type of mention
  //  type of antecedent mention (if link)
  def createFeatureIndex(docs: Seq[Document]): (scala.collection.mutable.Map[String, Int],Int) = {
    val features = scala.collection.mutable.Map[String, Int]()
    var ftInd = 0

    def addToMap(name: String): Unit = {
      if (!features.contains(name)) {
        features += name -> ftInd
        ftInd += 1
      }
    }

    for (doc: Document <- docs) {
      println("Begin feature extract: "+ doc.id)
      for (mention: FeatureSet <- doc.features) {
        for (antecedent: FeatureSet <- doc.features.slice(0, mention.refID+1)) {
         for(featName <- buildFeatSet(mention,antecedent)) {
           addToMap(featName)
         }
        }
      }
    }
    println(features.size+" reported size= "+ftInd)
    (features,ftInd)
  }

  def buildFeatSet(mention: FeatureSet, antecedent: FeatureSet): List[String] = {
    def parseMentionType(feat: FeatureSet): String = {
      if (feat.mentionType == "PRP")
        try {
          PronounDictionary.getCanonicalPronLc(feat.completeString)
        } catch{
          case e:Exception =>{
            println(feat.completeString)
            "PRP"
          }
        }
      else
        feat.mentionType
    }
    def buildName(name: String, ment: FeatureSet, ant: FeatureSet): String = {
      val anaphor = ment.refID != ant.refID

      val n = name + "mentT=" + parseMentionType(ment)
      if (anaphor) n + "antT=" + parseMentionType(ant) else n + ""
    }

    def nameOrSpeech(s: String, pos: String): String = {
      if (lexicalCounter.wordCountMap.getOrElse(s, 0) > 20) s
      else pos
    }

    val strings = scala.collection.mutable.MutableList[String]()
    val anaphor = mention.refID != antecedent.refID
    val length = mention.completeString.split(" ").length
    strings += buildName("anaph=" + anaphor + "hw=" + mention.semanticHead, mention, antecedent)
    if (!(length == 1)) {
      strings += buildName("anaph=" + anaphor + "firstWord=" + nameOrSpeech(mention.firstWord, mention.partsOfSpeech.first), mention, antecedent)
      strings += buildName("anaph=" + anaphor + "lastWord=" + nameOrSpeech(mention.lastWord, mention.partsOfSpeech.last), mention, antecedent)
    }
    strings += buildName("anaph=" + anaphor + "precedingWord=" + nameOrSpeech(mention.previousWord, mention.partsOfSpeech.prev), mention, antecedent)
    strings += buildName("anaph=" + anaphor + "followingword=" + nameOrSpeech(mention.nextWord, mention.partsOfSpeech.next), mention, antecedent)
    strings += buildName("anaph=" + anaphor + "mentLength=" + length, mention, antecedent)

    //features on antecedent(ONLY if a[i] != new
    if (anaphor) {
      val antLength = antecedent.completeString.split(" ").length
      strings += buildName("anthw=" + antecedent.semanticHead, mention, antecedent)
      if (!(antLength == 1)) {
        strings += buildName("antfirstWord=" + nameOrSpeech(antecedent.firstWord, antecedent.partsOfSpeech.first), mention, antecedent)
        strings += buildName("antlastWord=" + nameOrSpeech(antecedent.lastWord, antecedent.partsOfSpeech.last), mention, antecedent)
      }
      strings += buildName("antprecedingWord=" + nameOrSpeech(antecedent.previousWord, antecedent.partsOfSpeech.prev), mention, antecedent)
      strings += buildName("antfollowingword=" + nameOrSpeech(antecedent.nextWord, antecedent.partsOfSpeech.next), mention, antecedent)
      strings += buildName("antLength=" + antLength, mention, antecedent)

      //features on pair
      strings += buildName("exactStringMatch=" + (antecedent.completeString == mention.completeString), mention, antecedent)
      strings += buildName("headWordMatch=" + (antecedent.semanticHead == mention.semanticHead), mention, antecedent)
      strings += buildName("sentenceDistance=" + Math.min(mention.sentenceNum - antecedent.sentenceNum, 10), mention, antecedent)
      strings += buildName("mentionDistance=" + Math.min(mention.refID - antecedent.refID, 10), mention, antecedent)

    }
    strings.toList
  }
}
