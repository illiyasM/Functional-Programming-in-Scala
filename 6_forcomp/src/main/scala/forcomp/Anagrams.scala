package forcomp

object Anagrams {

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

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val my = y.toMap
    x.map(occ => (occ._1, occ._2 - (my.get(occ._1) getOrElse 0)) ).filter(_._2 > 0)
  }

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
        ) yield { j :: s }
      }
    }
    val intermediary = sentenceOccurrences(sentence)
    sentenceAnagramsHelper(intermediary)
  }
}
