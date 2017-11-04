package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isSemivowel
import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
trait Umlaut extends WordTransformation {

	def unapply(str: String): Option[String] = {

		val Syllables(syllables) = str

		val optNewSyllables = unapply(syllables)

		optNewSyllables.map(_.flatMap(_.letters).mkString)
	}

	def unapply(syllables: List[Syllable]): Option[List[Syllable]] = {

    val trigger = triggersIn(syllables.last).headOption

		val mapping = Seq(true, false).map(b => b -> getMapping(b, trigger).map{ case (k, v) => v -> k }).toMap

		val newSyllables = syllables.map(sy =>
		{
			val Syllable(onset, wholeNucleus, coda, isStressed) = sy

			val newNucleus =	mapping(sy.isStressed).getOrElse(wholeNucleus, wholeNucleus)

			Syllable(onset, newNucleus, coda, isStressed)
		})

    Some(newSyllables)
	}

	override def apply(syllables: List[Syllable]): List[Syllable] = syllables.map(sy =>
	{
    val trigger = triggersIn(syllables.last).headOption

		val mapping = getMapping(sy.isStressed, trigger)

    val Syllable(onset, wholeNucleus, coda, isStressed) = sy
		val vowelsOfNucleus = wholeNucleus filterNot isSemivowel
		val newNucleus =	mapping.getOrElse(vowelsOfNucleus, vowelsOfNucleus)

    Syllable(onset, wholeNucleus.replace(vowelsOfNucleus, newNucleus), coda, isStressed)
	})

	override def canTransform(syllables: List[Syllable]): Boolean = triggersIn(syllables.last).nonEmpty

  private def triggersIn(syllable: Syllable): Seq[Char] = {

    val letters = syllable.letters

    letters.filter(triggers.contains)
  }

	protected val triggers: Seq[Char]

	protected def getMapping(syllableIsStressed: Boolean, trigger: Option[Char]): Map[String, String]
}

class U_Umlaut extends Umlaut
{
	def getMapping(syllableIsStressed: Boolean, trigger: Option[Char]) = if(syllableIsStressed)
    trigger match {
      case Some('v') => vAugmentedTransformation
      case _ => umlautTransformStressed
    }
    else umlautTransformUnstressed

	protected val umlautTransformStressed = Map("a" -> "ǫ")

	private val umlautTransformUnstressed = Map("a" -> "u")

	override val triggers = Seq('u', 'v')

	override def toString = "U-umlaut"

  private val vAugmentedTransformation = Map(
    "a" -> "ǫ",
    "e" -> "ø",
    "é" -> "œ",
    "i" -> "y",
    "í" -> "ý"
  )
}

object Explicit_I_Umlaut extends Umlaut with Explicit {

	override val triggers = Seq('i', 'j')	// probably it's also pointless. Yeah, as I-umlaut is non-productive

	def getMapping(syllableIsStressed: Boolean, trigger: Option[Char]) = umlautTransformation

	private val umlautTransformation = Map(
		"a" -> "e",
		"á" -> "æ",
		"o" -> "ø",
		"ó" -> "œ",
		"ǫ" -> "ø",
		"u" -> "y",
		"ú" -> "ý",
		"au" -> "ey"
	)

	override def toString = "I-umlaut"
}

object U_Umlaut extends U_Umlaut
object Explicit_U_Umlaut extends U_Umlaut with Explicit