package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
trait Umlaut extends WordTransformation {

	def unapply(str: String): String = {

		val Syllables(syllables) = str

		val newSyllables = unapply(syllables)

		newSyllables.map(_.letters).reduce[String]{ case(a, b) => a + b }
	}

	def unapply(syllables: List[Syllable]): List[Syllable] = {

		val mapping = Seq(true, false).map(b => b -> getMapping(b).map{ case (k, v) => v -> k }).toMap

		syllables.map(sy =>
		{
			val nc1 = sy.nucleus
			val nc2 =	mapping(sy.isStressed).getOrElse(nc1, nc1)
			val newStr = sy.letters.replace(nc1, nc2)

			Syllable(newStr, sy.isStressed)
		})
	}

	override def forceApply(syllables: List[Syllable]): List[Syllable] = syllables.map(sy =>
	{
		val mapping = getMapping(sy.isStressed)

		val nc1 = sy.nucleus
		val nc2 =	mapping.getOrElse(nc1, nc1)
		val newStr = sy.letters.replace(nc1, nc2)

		Syllable(newStr, sy.isStressed)
	})

	override def isEligible(syllables: List[Syllable]): Boolean =	{

		val letters = syllables.last.letters
		triggers.exists(letters.contains(_))
	}

	protected val triggers: Seq[Char]

	protected def getMapping(syllableIsStressed: Boolean): Map[String, String]
}

class U_Umlaut extends Umlaut
{
	def getMapping(syllableIsStressed: Boolean) = if(syllableIsStressed) umlautTransformStressed else umlautTransformUnstressed

	private val umlautTransformStressed = Map(
		"a" -> "ö",
		"e" -> "ø",
		"é" -> "œ",
		"i" -> "y",
		"í" -> "ý"
	)

	private val umlautTransformUnstressed = Map("a" -> "u")

	override val triggers = Seq('u', 'v')

	override def toString = "U-umlaut"
}

class I_Umlaut extends Umlaut // 'with Explicit' should be here
{
	override val triggers = Seq('i', 'j')	// probably it's also pointless. Yeah, as I-umlaut is non-productive

	def getMapping(syllableIsStressed: Boolean) = umlautTransformation

	private val umlautTransformation = Map(
		"a" -> "e",
		"á" -> "æ",
		"o" -> "ø",
		"ó" -> "œ",
		"ö" -> "ø",
		"u" -> "y",
		"ú" -> "ý",
		"au" -> "ey"
	)

	override def toString = "I-umlaut"
}

object U_Umlaut extends U_Umlaut
object Explicit_I_Umlaut extends I_Umlaut with Explicit
object Explicit_U_Umlaut extends U_Umlaut with Explicit