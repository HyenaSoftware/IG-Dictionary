package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable.lengthOf
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant.isConsonant
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel._

import scala.annotation.tailrec

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



  trait MODE { def syllablesOf(word: String): List[Syllable] }

  object METRICAL extends MODE {

    override def syllablesOf(word: String): List[Syllable] = ???
  }

  object TRADITIONAL_IMP extends MODE {

    def syllablesOf(word: String): List[Syllable] = {

      // 0,4,6
      val syllableBegins = 0 +: word.zipWithIndex
        .filter { case (c, _) =>  isVowel(c) }
        .map { case (_, i) => i }
        .tail

      // 4,6
      val syllableBeginsShifted = syllableBegins.tail :+ word.length

      // (0,4), (4,6), (6,7)
      val syllableRanges = syllableBegins zip syllableBeginsShifted

      //
      val syllables = syllableRanges.map {

        case (0, e) => null
        case (s, e) =>
          val longestNucleus = word.substring(s, s + 2)
          val nucleus = if(isDiphtong(longestNucleus)) longestNucleus else word.substring(s, s + 1)
          val coda = word.substring(s + nucleus.length - 1, e)
          new Syllable("", nucleus, coda, true, Syllable.lengthOf(nucleus, coda))
      }

      syllables.toList
    }
  }

  object TRADITIONAL_REC extends MODE {

    val ONSET = 0
    val NUCLEUS = 1
    val CODA = 2

    private def saveAndBuildSyllable(parts: List[String], is: Int): Syllable = {

      val revParts = parts.reverse

      val onset = revParts.lift(0).getOrElse("")
      val nucleus = revParts.lift(1).getOrElse("")
      val coda = revParts.lift(2).getOrElse("")
      val length = Syllable.lengthOf(nucleus, coda)

      new Syllable(onset, nucleus, coda, onset.nonEmpty || is == 0, length)
    }

    private def concat(str: String, is: Int, ic: Int, parts: List[String]): List[String] = str.substring(is, ic) :: parts

    // kall-að-i
    @tailrec
    private def processNextChar(str: String, is: Int, ic: Int, parts: List[String], syllables: List[Syllable], mode: Int): List[Syllable] = {

      val c = str(ic)

      val inTheCorrectMode = mode match {

        case ONSET | CODA => isConsonant(c)
        case NUCLEUS => {
          val vs = str.substring(is, ic + 1)
          isVowel(vs)
        }
      }

      (mode, inTheCorrectMode) match {

        case _ if c == '$' =>
          saveAndBuildSyllable(concat(str, is, ic, parts), is) :: syllables

        // normal character processing
        case (_, true) => processNextChar(str, is, ic + 1, parts, syllables, mode)

        //
        case (ONSET,    false) => processNextChar(str, ic, ic, concat(str, is, ic, parts), syllables, NUCLEUS)
        case (NUCLEUS,  false) => processNextChar(str, ic, ic, concat(str, is, ic, parts), syllables, CODA)

        case (CODA, false) =>
          val sy = saveAndBuildSyllable(concat(str, is, ic, parts), is)
          processNextChar(str, ic, ic, List(""), sy :: syllables, NUCLEUS)

        case _ => syllables
      }
    }

    def syllablesOf(word: String): List[Syllable] = processNextChar(word + '$', 0, 0, List(), List(), ONSET).reverse
  }

  def syllablesOf(word: String, mode: MODE = TRADITIONAL_REC): List[Syllable] = mode.syllablesOf(word)

  def apply(syllables: List[Syllable]): String = syllables.flatMap(_.letters).mkString
}
