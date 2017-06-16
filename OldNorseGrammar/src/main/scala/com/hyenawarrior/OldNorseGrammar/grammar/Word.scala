package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.{I_Umlaut, U_Umlaut}

/**
	* Created by HyenaWarrior on 2017.03.01..
	*/
case class Word(pos: PoS)
{
	private val DEFAULT_TRANSFORMATIONS = List(U_Umlaut, I_Umlaut)
	private val POS_DEPENDENT_TRANSFORMATIONS = pos.transformations

	def underlyingPoS: PoS = pos

	// useful for lookup
	def strForm(): String =
	{
		val Syllables(syllables) = pos.strForm

		val allTransformations = POS_DEPENDENT_TRANSFORMATIONS ++ DEFAULT_TRANSFORMATIONS
		val transformedSyllables = allTransformations.foldLeft(syllables){ (sys, trn) => trn(sys) }

		val str = Syllables(transformedSyllables)

		str
	}

	// formatted description
	val description = "[not yet]"

	val traits: List[DescriptorFlag] = pos.descriptorFlags

	override def toString = s"$strForm [$pos + ${POS_DEPENDENT_TRANSFORMATIONS.map(_.toString)}]"
}

object Syllables {

	private val VOWELS = "aáeéiíoóöuú"

	def isVowel(c: Char): Boolean = VOWELS.exists(c == _)

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
		val vowelIndicies: List[Int] = word.zipWithIndex.filter{ case (c, _) => isVowel(c) }.map(_._2).toList

		// eliminate vowel sequences: haus-t, 23 -> 2
		val vowelIndiciesReduced = reduce(vowelIndicies)

		// (1,3), (4,5)
		val syllableEnds = vowelIndiciesReduced.drop(1).map(_ - 1) :+ word.length

		val syllables = split(word, 0, syllableEnds)

		Some(syllables)
	}

	def apply(syllables: List[Syllable]): String = syllables.map(_.letters).reduce((a, b) => a + b)

}





