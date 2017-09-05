import forcomp.Anagrams.sentenceAnagrams
import forcomp.loadDictionary
val sentence = List("Linux", "rulez")
val anas = List(
  List("Rex", "Lin", "Zulu"),
  List("nil", "Zulu", "Rex"),
  List("Rex", "nil", "Zulu"),
  List("Zulu", "Rex", "Lin"),
  List("null", "Uzi", "Rex"),
  List("Rex", "Zulu", "Lin"),
  List("Uzi", "null", "Rex"),
  List("Rex", "null", "Uzi"),
  List("null", "Rex", "Uzi"),
  List("Lin", "Rex", "Zulu"),
  List("nil", "Rex", "Zulu"),
  List("Rex", "Uzi", "null"),
  List("Rex", "Zulu", "nil"),
  List("Zulu", "Rex", "nil"),
  List("Zulu", "Lin", "Rex"),
  List("Lin", "Zulu", "Rex"),
  List("Uzi", "Rex", "null"),
  List("Zulu", "nil", "Rex"),
  List("rulez", "Linux"),
  List("Linux", "rulez")
)

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]
val dictionary: List[Word] = loadDictionary

def wordOccurrences(w: Word): Occurrences = w.toLowerCase.groupBy((x: Char) => x).map(y => (y._1, y._2.length)).toList.sortWith(_._1 < _._1)
def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))
lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(x => wordOccurrences(x))
def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
  case Nil => List(List())
  case x :: xs =>
    for {
      y <- combinations(xs)
      n <- 0 to x._2
    } yield if (n == 0) y else (x._1, n) :: y
}
def subtract(x: Occurrences, y: Occurrences): Occurrences = x.filterNot(y.map(_._1) contains _._1)

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def sentenceAnagramsHelper(o: Occurrences): List[Sentence] = {
    if (o.isEmpty) {
      List(Nil)
    } else {
      val combs = combinations(o)
      for (
        i <- combs if dictionaryByOccurrences.keySet(i);
        j <- dictionaryByOccurrences(i);
        s <- sentenceAnagramsHelper(subtract(o, i))
      ) yield {
        j :: s
      }
    }
  }

  sentenceAnagramsHelper(sentenceOccurrences(sentence))
}

combinations(sentenceOccurrences(sentence))
sentenceAnagrams(sentence)


