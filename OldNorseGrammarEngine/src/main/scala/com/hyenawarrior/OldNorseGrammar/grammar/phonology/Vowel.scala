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

  private val SEMIVOWELS = "jv"

  private val raising = Map('e' -> 'i', 'o' -> 'u')

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
    'á' -> Vowel(LOW  | BACK  | NON_LABIAL, LONG),
    'æ' -> Vowel(LOW  | FRONT | NON_LABIAL, SHORT),
    'e' -> Vowel(MID  | FRONT | NON_LABIAL, SHORT),
    'é' -> Vowel(MID  | FRONT | NON_LABIAL, LONG),
    'i' -> Vowel(HIGH | FRONT | NON_LABIAL, SHORT),
    'í' -> Vowel(HIGH | FRONT | NON_LABIAL, LONG),
    'o' -> Vowel(MID  | BACK  | LABIAL, SHORT),
    'ó' -> Vowel(MID  | BACK  | LABIAL, LONG),
    'ø' -> Vowel(MID  | FRONT | LABIAL, SHORT),
    'œ' -> Vowel(MID  | FRONT | LABIAL, LONG),
    'ǫ' -> Vowel(LOW  | BACK  | LABIAL, SHORT),
    'u' -> Vowel(HIGH | BACK  | LABIAL, SHORT),
    'ú' -> Vowel(HIGH | BACK  | LABIAL, LONG),
    'y' -> Vowel(HIGH | FRONT | LABIAL, SHORT),
    'ý' -> Vowel(HIGH | FRONT | LABIAL, LONG)
  )

	def isVowel(c: Char): Boolean = VOWEL_MAP contains c

	def isSemivowel(c: Char): Boolean = SEMIVOWELS contains c

	def isVowelOrSemivowel(c: Char): Boolean = isSemivowel(c) || isVowel(c)

	def raise(c: Char): Char = raising(c)

	def isBackVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isBack)

	def isLabialVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isLabial)

  def isShortVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isShort)

  def isAccentedVowel(c: Char): Boolean = VOWEL_MAP.get(c).exists(_.isLong)

  def isLow(c: Char) = VOWEL_MAP.get(c).exists(_.isLow)

  def isHigh(c: Char) = VOWEL_MAP.get(c).exists(_.isHigh)

  def apply(c: Char) = VOWEL_MAP(c)

  def unapply(c: Char): Option[(Int, Length)] = VOWEL_MAP.get(c).map {

    case Vowel(prop, length) => prop -> length
  }
}
