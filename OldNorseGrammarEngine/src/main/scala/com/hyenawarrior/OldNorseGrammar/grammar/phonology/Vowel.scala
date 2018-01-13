package com.hyenawarrior.OldNorseGrammar.grammar.phonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.Length


/**
	* Created by HyenaWarrior on 2017.10.15..
	*/
case class Vowel(prop: Int, length: Length) {

  def isBack  = (prop & 0x8) == Vowel.BACK
  def isFront = (prop & 0x8) == Vowel.FRONT

  def isLabial    = (prop & 0x4) == Vowel.LABIAL
  def isNonLabial = (prop & 0x4) == Vowel.NON_LABIAL

  def isLow   = (prop & 0x3) == Vowel.LOW
  def isHigh  = (prop & 0x3) ==  Vowel.HIGH

  def isShort = !length.isLong
  def isLong  = length.isLong
}

object Vowel {

  private val SEMIVOWELS = "jw"

  private val raising = Map('e' -> 'i', 'o' -> 'u')

  private val lengthening = Map('a' -> 'á', 'e' -> 'é', 'u' -> 'ú')
  private val shortening = lengthening.map { case (k, v) => v -> k }

  val LOW = 0x0   // also means it's open
  val MID = 0x1
  val HIGH = 0x2

  val NON_LABIAL = 0x0
  val LABIAL = 0x4

  val BACK = 0x0
  val FRONT = 0x8

  implicit class Length(val isLong: Boolean) extends AnyVal

  val SHORT: Length = false
  val LONG:  Length = true

  private val VOWEL_MAP: Map[Char, Vowel] = Map(
    'a' -> Vowel(LOW  | BACK  | NON_LABIAL, SHORT),
    // The long á was rounded early on, and may be pronounced as a long variant of ǫ (o-ogonek).
    'á' -> Vowel(LOW  | BACK  | LABIAL, LONG),
    // The letters æ and œ always represent long vowels, and do not therefore carry an accent mark.
    'æ' -> Vowel(LOW  | FRONT | NON_LABIAL, LONG),
    'e' -> Vowel(MID  | FRONT | NON_LABIAL, SHORT),
    'é' -> Vowel(MID  | FRONT | NON_LABIAL, LONG),
    'i' -> Vowel(HIGH | FRONT | NON_LABIAL, SHORT),
    'í' -> Vowel(HIGH | FRONT | NON_LABIAL, LONG),
    'o' -> Vowel(MID  | BACK  | LABIAL, SHORT),
    'ó' -> Vowel(MID  | BACK  | LABIAL, LONG),
    // The letters ø and ǫ (o-ogonek) always represent short vowels.
    'ø' -> Vowel(MID  | FRONT | LABIAL, SHORT),
    // The letters æ and œ always represent long vowels, and do not therefore carry an accent mark.
    'œ' -> Vowel(MID  | FRONT | LABIAL, LONG),
    // The letters ø and ǫ (o-ogonek) always represent short vowels.
    'ǫ' -> Vowel(LOW  | BACK  | LABIAL, SHORT),
    'u' -> Vowel(HIGH | BACK  | LABIAL, SHORT),
    'ú' -> Vowel(HIGH | BACK  | LABIAL, LONG),
    'y' -> Vowel(HIGH | FRONT | LABIAL, SHORT),
    'ý' -> Vowel(HIGH | FRONT | LABIAL, LONG)
  )

  def isLong(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isLong)

  def isShort(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isShort)

	def isVowel(c: Char): Boolean = VOWEL_MAP contains c

	def isSemivowel(c: Char): Boolean = SEMIVOWELS contains c

	def isVowelOrSemivowel(c: Char): Boolean = isSemivowel(c) || isVowel(c)

	def raise(c: Char): Char = raising(c)

	def isBackVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isBack)

  def isFrontVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isFront)

	def isLabialVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isLabial)

  def isShortVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isShort)

  def isAccentedVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isLong)

  def isLow(c: Char) = VOWEL_MAP.get(c).exists(_.isLow)

  def isHigh(c: Char) = VOWEL_MAP.get(c).exists(_.isHigh)

  def apply(c: Char) = VOWEL_MAP(c)

  def lengthen(c: Char) = lengthening.getOrElse(c, c)
  def shorten(c: Char) = shortening.getOrElse(c, c)

  def unapply(c: Char): Option[(Int, Length)] = VOWEL_MAP.get(c).map {

    case Vowel(prop, length) => prop -> length
  }
}
