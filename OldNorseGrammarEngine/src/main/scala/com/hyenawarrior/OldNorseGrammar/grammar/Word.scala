package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant.isConsonant
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowelOrSemivowel

import scala.language.postfixOps

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
case class Word(pos: PoS)
{
	private val DEFAULT_TRANSFORMATIONS = List(U_Umlaut)
	private val POS_DEPENDENT_TRANSFORMATIONS = pos.transformations

	def underlyingPoS: PoS = pos

	// useful for lookup
	def strForm(): String =
	{
		val Syllables(syllables) = pos.strForm

		val allTransformations = POS_DEPENDENT_TRANSFORMATIONS ++ DEFAULT_TRANSFORMATIONS
		val transformedSyllables = allTransformations.foldLeft(syllables) {

      case (sys, trn) if trn canTransform sys => trn(sys)
      case (sys, _) => sys
    }

		val str = Syllables(transformedSyllables)

		str

		//pos.strForm
	}

	// formatted description
	val description = "[not yet]"

	val traits: List[DescriptorFlag] = pos.descriptorFlags

	override def toString = s"${strForm()} [$pos + ${POS_DEPENDENT_TRANSFORMATIONS.map(_.toString)}]"
}

object Syllables {

  private def newMapBuffer(): Map[Char, StringBuilder] = Map(
    'o' -> StringBuilder.newBuilder,
    'n' -> StringBuilder.newBuilder,
    'c' -> StringBuilder.newBuilder)

  private def newSyllable(parts: Map[Char, StringBuilder], isStressed: Boolean) = {

    val onset = parts('o').result()
    val nucleus = parts('n').result()
    val coda = parts('c').result()

    Syllable(onset, nucleus, coda, isStressed)
  }

	private def parseWord(isStressed: Boolean, chars: List[(Char, Char)], parts: Map[Char, StringBuilder])
	: List[Syllable] = chars match {

    case (t @ ('c' | 'n'), k) :: (tail @ ('o', _) :: _) =>
      parts(t) += k
      newSyllable(parts, isStressed) :: parseWord(isStressed = false, tail, newMapBuffer())

    case (t, k) :: tail =>
      parts(t) += k
      parseWord(isStressed, tail, parts)

    case Nil => List(newSyllable(parts, isStressed))
  }

  def unapply(word: String): Option[List[Syllable]] =	{

    val firstOnsetIdx = word.indexWhere(isVowelOrSemivowel)

    val wordTail = word.toList.drop(firstOnsetIdx)

    val firstOnset = (1 to firstOnsetIdx).map(_ => 'o').toList

    val typesOfLetters = firstOnset ++ wordTail.map {
      case v if isVowelOrSemivowel(v) => 'n'
      case c if isConsonant(c) => 'c'
    }

    if(typesOfLetters.nonEmpty) {

      val typesOfLetters2 = typesOfLetters.zipAll(typesOfLetters.tail, ' ', ' ').map {

        case ('c', 'n') => 'o'
        case (a, _) => a
      }

      val chars = typesOfLetters2 zip word.toList

      val syllables = parseWord(isStressed = true, chars, newMapBuffer())

      Some(syllables)
    }
    else None
  }

  def apply(syllables: List[Syllable]): String = syllables.flatMap(_.letters).mkString
}





