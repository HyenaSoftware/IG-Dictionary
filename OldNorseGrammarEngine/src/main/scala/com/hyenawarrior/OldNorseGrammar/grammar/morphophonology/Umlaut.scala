package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isSemivowel
import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}

/**
  * Created by HyenaWarrior on 2017.04.15..
  */
trait Umlaut extends WordTransformation with InvertableTransformation {

  def unapply(str: String): Option[String] = {

    val Syllables(syllables) = str

    val optNewSyllables = unapply(syllables)

    optNewSyllables.map(_.flatMap(_.letters).mkString)
  }

  def unapply(syllables: List[Syllable]): Option[List[Syllable]] = {

    val mapping = Seq(true, false).map(b => b -> getMapping(b).map{ case (k, v) => v -> k }).toMap

    val newSyllables = syllables.map(sy =>
    {
      val Syllable(onset, wholeNucleus, coda, isStressed, length) = sy

      val newNucleus =	mapping(sy.isStressed).getOrElse(wholeNucleus, wholeNucleus)

      Syllable(onset, newNucleus, coda, isStressed, length)
    })

    Some(newSyllables)
  }

  override def apply(syllables: List[Syllable]): Option[List[Syllable]] = {

    val (first :: second, others) = syllables.splitAt(2)

    (transform(first), second.headOption.flatMap(transform)) match {

      case (Some(firstTr), None)					 => Some(firstTr +: second ::: others)
      case (Some(firstTr), Some(secondTr)) => Some(firstTr :: secondTr :: others)
      case _ => None
    }
  }

  private def transform(syllable: Syllable): Option[Syllable] = {

    val Syllable(onset, nucleus, coda, isStressed, length) = syllable
    val mapping = getMapping(isStressed)

    val vowelsOfNucleus = nucleus filterNot isSemivowel
    val optNewNucleus = mapping
      .get(vowelsOfNucleus)
      .map(nucleus.replace(vowelsOfNucleus, _))

    optNewNucleus.map(s => Syllable(onset, s, coda, isStressed, length))
  }

  override def canTransform(syllables: List[Syllable]): Boolean = triggersIn(syllables.last).nonEmpty

  private def triggersIn(syllable: Syllable): Seq[Char] = {

    val letters = syllable.nucleus + syllable.coda

    letters.filter(triggers.contains)
  }

  val triggers: Seq[Char]

  val targetVowels: Set[String]

  protected def getMapping(syllableIsStressed: Boolean): Map[String, String]
}

object I_Umlaut extends Umlaut {

  override val triggers = Seq('i', 'j')	// probably it's also pointless. Yeah, as I-umlaut is non-productive

  def getMapping(syllableIsStressed: Boolean) = umlautTransformation

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

  override val targetVowels = umlautTransformation.values.toSet
}

object U_Umlaut extends Umlaut {

  override val triggers = Seq('u')

  def getMapping(syllableIsStressed: Boolean) =
    if(syllableIsStressed) umlautTransformStressed
    else umlautTransformUnstressed

  protected val umlautTransformStressed = Map("a" -> "ǫ")

  private val umlautTransformUnstressed = Map("a" -> "u")

  override def toString = "U-umlaut"

  override val targetVowels = Set("ǫ")
}

object V_Umlaut extends Umlaut {

  override val triggers = Seq('v')

  def getMapping(syllableIsStressed: Boolean) = if(syllableIsStressed) vAugmentedTransformation
  else umlautTransformUnstressed

  private val vAugmentedTransformation = Map(
    "a" -> "ǫ",
    "e" -> "ø",
    "é" -> "œ",
    "i" -> "y",
    "í" -> "ý"
  )

  private val umlautTransformUnstressed = Map("a" -> "u")

  override def toString = "U-umlaut"

  override val targetVowels = vAugmentedTransformation.values.toSet
}
