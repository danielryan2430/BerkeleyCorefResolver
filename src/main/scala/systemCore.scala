/**
 * Created by dimberman on 12/11/14.
 */
import java.io.File
object systemCore {

  def goldFileList(file:File):Seq[File]={
    (Seq[File]() /: file.listFiles){ (files:Seq[File],newFile:File) =>
      if(newFile.isDirectory)
        files ++ goldFileList(newFile)
      else if(newFile.getName.contains(".gold_conll")    )
        files:+newFile
      else
        files}
  }

  def main(args: Array[String]){
         val trainingDataPath = args(0)
         val testingDataPath = args(1)

  }
}
