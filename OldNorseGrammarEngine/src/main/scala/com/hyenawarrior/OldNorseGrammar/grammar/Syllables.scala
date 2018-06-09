package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._
import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.lengthOf

/**
  * Created by HyenaWarrior on 2018.06.09..
  */
object Syllables {

  private def findWhere(word: String, predicate: Char => Boolean, from: Int): Int =
    word.indexWhere(predicate, from) match {

      case -1 => word.length
      case i => i
    }

  private def fetchNextPair(word: String, i: Int): (String, String, Int) = {

    val vowelStartIdx = findWhere(word, isVowel, i)

    val consStartIdx = vowelStartIdx + {
      val remains = word substring vowelStartIdx

      if (isDiphtong(remains take 2)) 2
      else if (isVowel(remains.head)) 1
      else 0
    }

    val consEndIdx    = findWhere(word, isVowel, consStartIdx)

    val vowels = word.substring(vowelStartIdx, consStartIdx)

    val consonants = word.substring(consStartIdx, consEndIdx)

    (vowels, consonants, consEndIdx)
  }

  private def iterateOverTheWord(word: String, i: Int): List[(String, String)] = if(word.length > i) {

    val (nucleus, coda, j) = fetchNextPair(word, i)

    (nucleus, coda) +: iterateOverTheWord(word, j)

  } else List()

  private def adjustAndCraeteSyllables(nextOnset: String, parts: List[(String, String)]
                                       , isStressed: Boolean = false): List[Syllable] = parts match {

    case (nucleus, coda) :: Nil =>
    List(Syllable(nextOnset, nucleus, coda, isStressed, lengthOf(nucleus, coda)))

    case (nucleus, coda) :: other =>
    val adjustedCoda = coda dropRight 1
    val syllableLength = lengthOf(nucleus, adjustedCoda)

    Syllable(nextOnset, nucleus, adjustedCoda, isStressed, syllableLength) +:
      adjustAndCraeteSyllables(coda takeRight 1, other)
  }

  def unapply(word: String): Option[List[Syllable]] = if(word.isEmpty) None else	{

    val firstVowelIndex = word indexWhere isVowel
    val firstOnset = word.substring(0, firstVowelIndex)
    val parts      = iterateOverTheWord(word, firstVowelIndex)

    Some(adjustAndCraeteSyllables(firstOnset, parts, isStressed = true))
  }

  def apply(syllables: List[Syllable]): String = syllables.flatMap(_.letters).mkString
}
