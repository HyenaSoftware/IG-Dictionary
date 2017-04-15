package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.Syllable

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
trait Umlaut extends WordTransformation {

	override def forceApply(syllables: List[Syllable]): List[Syllable] = syllables.map
	{
		case Syllable(str, flag) =>
			val str2 = str.map(c => umlautTransformation.get(c).map(e => selector(e, flag)).getOrElse(c))
			Syllable(str2, flag)
	}

	private def selector(e: (Char, Char), isStressed: Boolean): Char = if(isStressed)	e._1 else e._2

	override protected def isEligible(syllables: List[Syllable]): Boolean =	{

		val lastSyl = syllables.last
		lastSyl.letters.exists(c => c == trigger)
	}

	val trigger: Char
	val umlautTransformation: Map[Char, (Char, Char)]
}

object U_Umlaut extends Umlaut {

	override val umlautTransformation = Map('a' -> ('ö' -> 'u'))
	override val trigger = 'u'

	override def toString = "U-umlaut"
}

object I_Umlaut extends Umlaut {

	override val trigger = 'i'

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