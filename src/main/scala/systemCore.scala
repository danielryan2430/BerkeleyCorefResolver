/**
 * Created by dimberman on 12/11/14.
 */
import java.io.File
import resolver.parser.document.{Parser, Document}
import classifier.{FeatureExtractor,adaGradTrainer,bayesianClassifier}
object systemCore {

  def getTotalErrors(chain:Seq[Int], doc:Document):Int={
    (0 /: chain.zip(doc.features)){
      case (errors,(ass,fs)) => if(fs.mentionID==doc.features(ass).mentionID) 0 else 1
    }
  }

  def DetailedLoss(chain:Seq[Int], doc:Document):(Int,Int,Int,Int)={
    val lssFn = adaGradTrainer.lossFunctionGen(1,2,3)
    ((0,0,0,0) /: chain.zipWithIndex){
      case (errors,(assn,ind)) => {val s = lssFn(doc,ind,assn)
        if(s==1)
          (errors._1+1,errors._2,errors._3,errors._4)
        else if(s==2)
          (errors._1,errors._2+2,errors._3,errors._4)
        else if(s==3)
          (errors._1,errors._2,errors._3+1,errors._4)
        else
          (errors._1,errors._2,errors._3,errors._4+1)
      }
    }
  }

  def goldFileList(file:File):Seq[File]= {
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

  def main(args: Array[String]){
         val trainingDataPath = "/Users/dimberman/Code/NLP/conll-2011/v2/data/train"
         val testingDataPath = "/Users/dimberman/Code/NLP/conll-2011/v2/data/test"

    val goldTrainFiles = goldFileList(new File(trainingDataPath))
    /* do parsiing here*/
    if(goldTrainFiles.length == 0) print("i like apples")
    val processedDocs = goldTrainFiles.map(Parser.parse(_))

    val featurizer = new FeatureExtractor(processedDocs)

    val weights = adaGradTrainer.train(processedDocs,.1,.001,10,featurizer.featCount,featurizer.extractFeatures,adaGradTrainer.lossFunctionGen(.1,3,1))

    val goldTestFiles = goldFileList(new File(testingDataPath))
    /*Do Parsing here*/
    val testingDocs = goldTestFiles.map(Parser.parse(_))
    val entries = testingDocs.map(bayesianClassifier.classify(weights,_,featurizer.extractFeatures))
    val totalErrors = (0 /:entries.zip(testingDocs)){case (error,(assn,doc))=>error+getTotalErrors(assn,doc)}
    println("Total Errors = "+totalErrors)
    val (fn,fa,wl,corr) = ((0,0,0,0) /:entries.zip(testingDocs)){case (error,(assn,doc))=>val dl = DetailedLoss(assn,doc)
      (error._1+dl._1,error._2+dl._2,error._3+dl._3,error._4+dl._4)}
    println("False New = "+fn)
    println("False Anaphor = "+fa)
    println("Wrong Link = "+wl)
    println("Correctly Assigned Links = "+corr)


  }
}
