package resolver.parser.document

import scala.collection.mutable.HashMap

/**
 * we took this class from the source code for the berkeley coref project, since they did not define the kind of pronoun resolution used;
 * To this end, it seemed like the correct strategy was to simply link their pronoun resolution scheme, as otherwise reimplmenting it would
 * have yielded the exact identical structure.
 */
object PronounDictionary {
  val firstPersonPronouns = Set("i", "me", "myself", "mine", "my", "we", "us", "ourself", "ourselves", "ours", "our");
  val secondPersonPronouns = Set("you", "yourself", "yours", "your", "yourselves");
  val thirdPersonPronouns = Set("he", "him", "himself", "his", "she", "her", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's", "they", "them", "themself", "themselves", "theirs", "their", "they", "them", "'em", "themselves");
  val otherPronouns = Set("who", "whom", "whose", "where", "when","which");
  
  // Borrowed from Stanford
  val singularPronouns = Set("i", "me", "myself", "mine", "my", "yourself", "he", "him", "himself", "his", "she", "her", "herself", "hers", "her", "it", "itself", "its", "one", "oneself", "one's");
  val pluralPronouns = Set("we", "us", "ourself", "ourselves", "ours", "our", "yourself", "yourselves", "they", "them", "themself", "themselves", "theirs", "their");
  val malePronouns = Set("he", "him", "himself", "his");
  val femalePronouns = Set("her", "hers", "herself", "she");
  val neutralPronouns = Set("it", "its", "itself", "where", "here", "there", "which");
  
  
  val allPronouns = firstPersonPronouns ++ secondPersonPronouns ++ thirdPersonPronouns ++ otherPronouns;

  // Constructed based on Stanford's Dictionaries class
  val pronounsToCanonicalPronouns = new HashMap[String,String]();
  pronounsToCanonicalPronouns.put("i", "i");
  pronounsToCanonicalPronouns.put("me", "i");
  pronounsToCanonicalPronouns.put("my", "i");
  pronounsToCanonicalPronouns.put("myself", "i");
  pronounsToCanonicalPronouns.put("mine", "i");
  pronounsToCanonicalPronouns.put("you", "you");
  pronounsToCanonicalPronouns.put("your", "you");
  pronounsToCanonicalPronouns.put("yourself", "you");
  pronounsToCanonicalPronouns.put("yourselves", "you");
  pronounsToCanonicalPronouns.put("yours", "you");
  pronounsToCanonicalPronouns.put("he", "he");
  pronounsToCanonicalPronouns.put("him", "he");
  pronounsToCanonicalPronouns.put("his", "he");
  pronounsToCanonicalPronouns.put("himself", "he");
  pronounsToCanonicalPronouns.put("she", "she");
  pronounsToCanonicalPronouns.put("her", "she");
  pronounsToCanonicalPronouns.put("herself", "she");
  pronounsToCanonicalPronouns.put("hers", "she");
  
  pronounsToCanonicalPronouns.put("we", "we");
  pronounsToCanonicalPronouns.put("us", "we");
  pronounsToCanonicalPronouns.put("our", "we");
  pronounsToCanonicalPronouns.put("ourself", "we");
  pronounsToCanonicalPronouns.put("ourselves", "we");
  pronounsToCanonicalPronouns.put("ours", "we");
  pronounsToCanonicalPronouns.put("they", "they");
  pronounsToCanonicalPronouns.put("them", "they");
  pronounsToCanonicalPronouns.put("their", "they");
  pronounsToCanonicalPronouns.put("themself", "they");
  pronounsToCanonicalPronouns.put("themselves", "they");
  pronounsToCanonicalPronouns.put("theirs", "they");
  pronounsToCanonicalPronouns.put("'em", "they");
  pronounsToCanonicalPronouns.put("it", "it");
  pronounsToCanonicalPronouns.put("itself", "it");
  pronounsToCanonicalPronouns.put("its", "it");
  pronounsToCanonicalPronouns.put("one", "one");
  pronounsToCanonicalPronouns.put("oneself", "one");
  pronounsToCanonicalPronouns.put("one's", "one");
  
  pronounsToCanonicalPronouns.put("that", "that");
  pronounsToCanonicalPronouns.put("which", "which");
  pronounsToCanonicalPronouns.put("who", "who");
  pronounsToCanonicalPronouns.put("whom", "who");
//  pronounsToCanonicalPronouns.put("where", "where");
//  pronounsToCanonicalPronouns.put("whose", "whose");
//  This entry is here just to make results consistent with earlier ones
//  on our very small dev set
  pronounsToCanonicalPronouns.put("thy", "thy");
  pronounsToCanonicalPronouns.put("y'all", "you");
  pronounsToCanonicalPronouns.put("you're", "you");
  pronounsToCanonicalPronouns.put("you'll", "you");
  pronounsToCanonicalPronouns.put("'s", "'s");
  
  def isPronLc(str: String): Boolean = {
    allPronouns.contains(str.toLowerCase());
  }
  
  def getCanonicalPronLc(str: String): String = {
    if (!pronounsToCanonicalPronouns.contains(str.toLowerCase())) {
      "";
    } else {
      pronounsToCanonicalPronouns(str.toLowerCase());
    }
  }

}