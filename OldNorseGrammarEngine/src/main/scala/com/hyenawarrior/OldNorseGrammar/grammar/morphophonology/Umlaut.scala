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

		syllables.map
		{
			case Syllable(str, flag) =>
				val str2 = str.map(c => inverseUmlautTransform.getOrElse(c, c))

				Syllable(str2, flag)
		}
	}

	override def forceApply(syllables: List[Syllable]): List[Syllable] = syllables.map
	{
		case Syllable(str, isStressed) =>
			val str2 = str.map(c => umlautTransformation.get(c).map(e => selector(e, isStressed)).getOrElse(c))
			Syllable(str2, isStressed)
	}

	private def selector(e: (Char, Char), isStressed: Boolean): Char = if(isStressed)	e._1 else e._2

	override def isEligible(syllables: List[Syllable]): Boolean =	{

		val lastSyl = syllables.last
		lastSyl.letters.exists(c => c == trigger)
	}

	val trigger: Char
	val umlautTransformation: Map[Char, (Char, Char)]
	lazy val inverseUmlautTransform = umlautTransformation.flatMap { case (a, (b, c)) => Seq(b -> a, c -> a) }
}

class U_Umlaut extends Umlaut
{
	override val umlautTransformation = Map('a' -> ('ö' -> 'u'))
	override val trigger = 'u'

	override def toString = "U-umlaut"
}

class I_Umlaut extends Umlaut // 'with Explicit' should be here
{
	override val trigger = 'i'	// probably it's also pointless

	override val umlautTransformation = Map(
		'a' -> ('e' -> 'e'),
		'á' -> ('æ' -> 'æ'),
		'o' -> ('ø' -> 'ø'),
		'ó' -> ('œ' -> 'œ'),
		'ö' -> ('ø' -> 'ø'),
		'u' -> ('y' -> 'y'),
		'ú' -> ('ý' -> 'ý')
		//"au" -> "ey"
	)

	override def toString = "I-umlaut"
}

object I_Umlaut extends I_Umlaut  // it may not be correct, I-umlaut is a non-productive feature of ON
object U_Umlaut extends U_Umlaut
object Explicit_I_Umlaut extends I_Umlaut with Explicit
object Explicit_U_Umlaut extends U_Umlaut with Explicit