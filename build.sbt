name := "SBTCorefResolver"

version := "1.0"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0"

libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.5.0" classifier "models"

libraryDependencies += "org.apache.lucene" % "lucene-wordnet" % "3.2.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

//libraryDependencies += "edu.washington.cs.knowitall.stanford-corenlp" % "stanford-ner-models" % "1.3.5"
