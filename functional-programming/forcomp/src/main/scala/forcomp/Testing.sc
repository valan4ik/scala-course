import scala.collection.immutable._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    *  how often the character appears.
    *  This list is sorted alphabetically w.r.t. to the character in each pair.
    *  All characters in the occurrence list are lowercase.
    *
    *  Any list of pairs of lowercase characters and their frequency which is not sorted
    *  is **not** an occurrence list.
    *
    *  Note: If the frequency of some character is zero, then that character should not be
    *  in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary


  def wordOccurrences(w: Word): Occurrences = {
    (w.toLowerCase groupBy (_.toChar) map (p => (p._1, p._2.length)) toList) sortWith (_._1 < _._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))


  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = // dictionary groupBy (wordOccurrences(_))
    (dictionary map (word => (wordOccurrences(word), word))) groupBy (n => n._1) mapValues (x => x map (y => y._2)) withDefaultValue List()

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))


  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty) List(List())
    else {
      val head = occurrences.head
      val combos: List[Occurrences] = combinations(occurrences.tail)
      (for {
        combo <- combos
        n <- 1 to head._2
      } yield (head._1, n) :: combo) ++ combos
    }
  }

//  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
//    y.foldLeft(x.toMap)((second, first) =>
//      if (first._2 == second(first._1)) second - first._1
//      else second updated (first._1, second(first._1) - first._2)
//    ).toList
//  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    (y foldLeft SortedMap[Char,Int]() ++ x){ case (map, (ch, tm)) => {
      val newTm = map(ch) - tm
      if (newTm != 0) map updated (ch, newTm)
      else map - ch
    } }.toList
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def helper(sent: Occurrences): List[Sentence] = {
      if (sent.isEmpty) List(List())
      else {
        for {
          comb <- combinations(sent)
          word <- dictionaryByOccurrences(comb)
          rest <- helper(subtract(sent,comb))
        } yield word::rest
      }
    }
    helper(sentenceOccurrences(sentence))
  }
}

Anagrams.subtract(Anagrams.sentenceOccurrences(List("Linux", "rulez")), Anagrams.wordOccurrences("lin"))
Anagrams.sentenceAnagrams(List("Linux", "rulez"))