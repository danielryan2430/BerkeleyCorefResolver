

import java.io.{FileWriter, File}
import resolver.parser.document.{Parser, Document}
import classifier.{FeatureExtractor, adaGradTrainer, bayesianClassifier}

import scala.util.Random

object systemCore {

  def DetailedLoss(chain: Seq[Int], doc: Document): (Int, Int, Int, Int) = {
    val lssFn = adaGradTrainer.lossFunctionGen(1, 2, 3)
    ((0, 0, 0, 0) /: chain.zipWithIndex) {
      case (errors, (assn, ind)) => {
        val s = lssFn(doc, ind, assn)
        if (s == 1)
          (errors._1 + 1, errors._2, errors._3, errors._4)
        else if (s == 2)
          (errors._1, errors._2 + 2, errors._3, errors._4)
        else if (s == 3)
          (errors._1, errors._2, errors._3 + 1, errors._4)
        else
          (errors._1, errors._2, errors._3, errors._4 + 1)
      }
    }
  }

  def goldFileList(file: File): Seq[File] = {
    // if (file.isDirectory) {
    (Seq[File]() /: file.listFiles) { (files: Seq[File], newFile: File) =>
      if (newFile.isDirectory) {
        files ++ goldFileList(newFile)
      }
      else if (newFile.getName.contains("gold_conll"))
        files :+ newFile
      else
        files
    }
    // } else List(file)
  }

  def main(args: Array[String]) {
    val trainingDataPath = "./conll-2011/v2/data/train/data/english/annotations/bn"
    val testingDataPath = "./conll-2011_test_key/v2/data/test/data/english/annotations/bn"

    val goldTrainFiles = goldFileList(new File(trainingDataPath))
    /* do parsiing here*/
    if (goldTrainFiles.length == 0) print("i like apples")
    val processedDocs: List[List[Document]] = goldTrainFiles.map(Parser.parse(_)).toList
    println("number of documents: " + processedDocs.length)

//    for(doc <-processedDocs){
//      println("individual length "-> doc.length)
//    }
    val docList = processedDocs.flatten //.foldLeft(List[Document]())((d: List[Document], b: List[Document]) => d ++ b)
    println("number of mentions: " + (0 /: docList){(a,b)=> a+b.features.length})

    val featurizer = new FeatureExtractor(docList)

    featurizer.dumpFeatures()

    //random tests if true
    val noWeights = false



    var weights = Array[Double]()
    if(!noWeights) {
      weights = adaGradTrainer.train(docList, 1, .001, 5, featurizer.featCount, featurizer.extractFeatures, adaGradTrainer.lossFunctionGen(3, .1, 1))


      val sav = new File("./SaveWeights.txt")
      val wri = new FileWriter(sav)
      weights.dropRight(1).map(a => wri.write(a + ","))
      wri.write(weights.last.toString)
      wri.close()
    }

    println("number of weights to consider: " + weights.length)
    println("weight nonzero count: "+(0/:weights){(a,b)=>if (b>1e-6 || b< -1e-6) a+1 else a})
    val goldTestFiles = goldFileList(new File(testingDataPath))
    /*Do Parsing here*/
    val testingDocs = goldTestFiles.map(Parser.parse(_)).toList
    val testDocList = testingDocs.foldLeft(List[Document]())((d: List[Document], b: List[Document]) => d ++ b)

    if(noWeights) {
      var fnt =0
      var wlt =0
      var fat =0
      var corrt=0;
      for (i <- List.fill(25)(0)) {
        weights = Array.fill(featurizer.featCount)(0.0)
        weights =weights.map(a =>Random.nextDouble())
        val entries = testDocList.map(bayesianClassifier.classify(weights, _, featurizer.extractFeatures))
        val (fn, fa, wl, corr) = ((0, 0, 0, 0) /: entries.zip(testDocList)) { case (error, (assn, doc)) => val dl = DetailedLoss(assn, doc)
          (error._1 + dl._1, error._2 + dl._2, error._3 + dl._3, error._4 + dl._4)
        }
        fnt+=fn
        wlt+=wl
        fat+=fa
        corrt+=corr
      }
      println("25 pt averages")
      println("False New avg= " + fnt/25. +" Total: "+fnt)
      println("False Anaphor avg= " + fat/25. +" Total: "+fat)
      println("Wrong Link avg= " + wlt/25.  +" Total: "+wlt)
      println("Correctly Assigned Links avg= " + corrt/25. +" Total: "+corrt)
    }

    val entries = testDocList.map(bayesianClassifier.classify(weights, _, featurizer.extractFeatures))

    val (fn, fa, wl, corr) = ((0, 0, 0, 0) /: entries.zip(testDocList)) { case (error, (assn, doc)) => val dl = DetailedLoss(assn, doc)
      (error._1 + dl._1, error._2 + dl._2, error._3 + dl._3, error._4 + dl._4)
    }
//    val a =  scala.collection.mutable.Set[Int]()
//    for(doc <- docList){
//      for(feature <- doc.features){
//           a.add(feature.mentionID)
//      }
//    }
//    println("number of gold clusters: " + a.size)
    println("Total Errors = " + (fn+fa+wl))

    println("False New = " + fn)
    println("False Anaphor = " + fa)
    println("Wrong Link = " + wl)
    println("Correctly Assigned Links = " + corr)
    println("efficacy % = " + (corr*1.0/(corr+fn+fa+wl))*100.)

  }
}
