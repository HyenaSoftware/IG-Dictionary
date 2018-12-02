package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
  * Created by HyenaWarrior on 2017.10.15..
  */
object Consonant {

  private val CONSONANTS 		= Set('b', 'd', 'ð', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'r', 's', 't', 'v', 'x', 'z', 'þ')

  private val nasals				= Set('n', 'm')

  private val voiced        = Set('b', 'd', 'g', 'm', 'n', 'r', 'l')  // f/v, þ/ð
  private val voicedStop		= Set('b', 'd', 'g')
  private val voicelessStop = Set('p', 't', 'k')

  private val dentals = Set('ð', 'd', 't', 's')

  private val devoicing = Map('b' -> 'p', 'd' -> 't', 'g' -> 'k')
  private val voicing   = Map('p' -> 'b', 't' -> 'd', 'k' -> 'g')

  private val velars		= Set('k', 'g', 'h')

  def isConsonant(c: Char) = CONSONANTS contains c

  def isNasal(c: Char): Boolean = nasals contains c

  def isVoiced(c: Char): Boolean = voiced contains c
  def isVoiceless(c: Char): Boolean = isConsonant(c) && !isVoiced(c)

  def isVoicedStop(c: Char): Boolean = voicedStop contains c

  def isVoicelessStop(c: Char): Boolean = voicelessStop contains c

  def isDental(c: Char) = dentals contains c

  def isVelar(c: Char) = velars contains c

  def devoice(c: Char): Char = devoicing(c)
  def voice(c: Char): Char = voicing(c)
}

abstract class AbstractConsonant(c: Char, phonemeProperty: PhonemeProperty) extends Phoneme {

  override def isConsonant: Boolean = true

  override def asShortened: Option[Phoneme] = ???

  override def asLengthened: Option[Phoneme] = ???

  def isDigraph: Boolean

  override def lengthInLetters: Int = 1
}

final case class Consonant(c: Char, phonemeProperty: PhonemeProperty) extends AbstractConsonant(c, phonemeProperty) {

  override def copyWithPropertyOf(newPhonemeProperty: PhonemeProperty) = Consonant(c, newPhonemeProperty)

  override def toString: String = s"$c:1"

  override def isDigraph = false

  override val asString: String = c.toString
}

trait DigraphLetterOrder
object FirstDigraphLetter extends DigraphLetterOrder
object SecondDigraphLetter extends DigraphLetterOrder

final case class HalfDigraph(c: Char, order: DigraphLetterOrder, phonemeProperty: PhonemeProperty) extends AbstractConsonant(c, phonemeProperty) {

  override def copyWithPropertyOf(newPhonemeProperty: PhonemeProperty) = HalfDigraph(c, order, newPhonemeProperty)

  override def toString = (c, order) match {
    case ('z', FirstDigraphLetter) => "t:1"
    case ('x', FirstDigraphLetter) => "k:1"
    case ('z' | 'x', SecondDigraphLetter) => "s:1"
  }

  override val asString: String = (c, order) match {
    case ('z', FirstDigraphLetter) => "t"
    case ('x', FirstDigraphLetter) => "k"
    case ('z' | 'x', SecondDigraphLetter) => "s"
  }

  override def isDigraph = true
}
