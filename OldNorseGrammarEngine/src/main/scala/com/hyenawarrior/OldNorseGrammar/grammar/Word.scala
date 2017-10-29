package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowelOrSemivowel

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

	override def toString = s"$strForm [$pos + ${POS_DEPENDENT_TRANSFORMATIONS.map(_.toString)}]"
}

object Syllables {



	private def split(word: String, i: Int, syllableEnds: Seq[Int]): List[Syllable] = syllableEnds.headOption match
	{
		case Some(beforeNextNuc) =>

			val syllable = word.substring(i, beforeNextNuc)
			val otherSyllables = split(word, beforeNextNuc, syllableEnds.tail)

			Syllable(syllable, i==0) +: otherSyllables

		case None => List()
	}

	private def reduce(nucleus: List[Int]): List[Int] = nucleus match
	{
		case n :: m :: tail if n+1==m => reduce(n::tail)
		case n :: tail                => n :: reduce(tail)
		case Nil                      => List()
	}

	def unapply(word: String): Option[List[Syllable]] =	{

		// kas-ta, kal-la-ði
		// 14, 146
		val vowelIndicies: List[Int] = word.zipWithIndex.filter {
			/* 	'v' represent both the consonant 'v' and the semivowel 'w' as in 'verða' and 'syngva'
					but as it never occures as ablaut of strong verbs, and semivowel deletion rule sometimes removes 'v's,
					which should not be considered semivowels.
			*/
			case ('v', _) => false
			case (c, _) => isVowelOrSemivowel(c)
		}.map(_._2).toList

		// eliminate vowel sequences: haus-t, 23 -> 2
		val vowelIndiciesReduced = reduce(vowelIndicies)

		// (1,3), (4,5)
		val syllableEnds = vowelIndiciesReduced.drop(1).map(_ - 1) :+ word.length

		val syllables = split(word, 0, syllableEnds)

		Some(syllables)
	}

	def apply(syllables: List[Syllable]): String = syllables.map(_.letters).reduce((a, b) => a + b)

}





