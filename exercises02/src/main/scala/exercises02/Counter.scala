package exercises02

import scala.util.matching.Regex

object Counter {

  /**
    * Посчитать количество вхождений слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  private val regexWords: Regex                  = "[^\\s\\n\\t\\r!(),.:?]+".r
  def countWords(text: String): Map[String, Int] = createMap(text, regexWords)

  /**
    * Посчитать количество вхождений английских слов в тексте
    * слово отделено символами [\s.,!?:\n\t\r]
    */
  private val regexEnWords: Regex                       = "[^\\s\\n\\t\\r!(),.:?]+[a-zA-Z-']+".r
  def countEnglishWords(text: String): Map[String, Int] = createMap(text, regexEnWords)

  /**
    * Посчитать количество вхождений чисел в тексте
    * число отделено символами [\s!?:\n\t\r]
    */
  private val regexNumbers: Regex                  = "\\d+([.,]\\d+)?".r
  def countNumbers(text: String): Map[String, Int] = createMap(text, regexNumbers)

  private def createMap(text: String, regex: Regex): Map[String, Int] =
    regex.findAllIn(text.toLowerCase).foldLeft(Map.empty[String, Int]) { (countEnWords, word) =>
      countEnWords + (word -> (countEnWords.getOrElse(word, 0) + 1))
    }

}
