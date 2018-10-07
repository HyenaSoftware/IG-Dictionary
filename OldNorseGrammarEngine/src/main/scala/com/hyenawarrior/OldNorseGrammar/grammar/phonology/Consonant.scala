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

final case class Consonant(cs: String, phonemeProperty: PhonemeProperty) extends Phoneme {

  override def isConsonant: Boolean = true

  override def asShortened: Option[Phoneme] = ???

  override def asLengthened: Option[Phoneme] = ???

  override def copyWithPropertyOf(phonemeProperty: PhonemeProperty) = Consonant(cs, phonemeProperty)

  override val lengthInLetters: Int = cs.length

  override val asString: String = cs

  override def toString: String = s"$cs:$lengthInLetters"
}