package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
  * Created by HyenaWarrior on 2018.06.09..
  */
trait Vowel2 extends Phoneme {

  def asFrontMutated: Option[Vowel2] = ???
  def asBackMutated: Option[Vowel2] = ???
  def asShortened: Option[Vowel2] = ???
  def asLengthened: Option[Vowel2] = ???

  def isFrontMutated: Boolean = ???

  def isLong: Boolean

  def copyWithPropertyOf(phonemeProperty: PhonemeProperty): Vowel2

  override def isVowel = true
}

object Vowel2 {

  trait Property

  class VowelProperty(bitField: Byte) extends Property {

    override def toString: String = {

      import Property._

      val high = bitField & HEIGHT_MAKS match {
        case LOW => "LOW"
        case MID => "MID"
        case HIGH => "HIGH"
      }

      val labial = if((bitField & LABIAL) != 0) "LABIAL" else "NONLABIAL"
      val front = if((bitField & FRONT) != 0) "FRONT" else "BACK"
      val long = if((bitField & LONG) != 0) "LONG" else "SHORT"

      s"[$high][$long][$front][$labial]"
    }
  }

  object DiphthongProperty extends Property

  object Property {

    val LOW = 0x0.toByte
    val MID = 0x1.toByte
    val HIGH = 0x2.toByte
    val HEIGHT_MAKS = 0x3.toByte

    val NON_LABIAL = 0x0.toByte
    val LABIAL = (0x1 << 2).toByte

    val BACK = 0x0.toByte
    val FRONT = (0x1 << 4).toByte

    val SHORT = (0x1 << 5).toByte
    val LONG = (0x1 << 5).toByte
  }

  import Property._

  private val VOWEL_MAP: Map[Char, VowelProperty] = Map(
    'a' -> new VowelProperty((LOW  | BACK  | NON_LABIAL | SHORT).toByte),
    // The long á was rounded early on, and may be pronounced as a long variant of ǫ (o-ogonek).
    'á' -> new VowelProperty((LOW  | BACK  | LABIAL | LONG).toByte),
    // The letters æ and œ always represent long vowels, and do not therefore carry an accent mark.
    'æ' -> new VowelProperty((LOW  | FRONT | NON_LABIAL | LONG).toByte),
    'e' -> new VowelProperty((MID  | FRONT | NON_LABIAL | SHORT).toByte),
    'é' -> new VowelProperty((MID  | FRONT | NON_LABIAL | LONG).toByte),
    'i' -> new VowelProperty((HIGH | FRONT | NON_LABIAL | SHORT).toByte),
    'í' -> new VowelProperty((HIGH | FRONT | NON_LABIAL | LONG).toByte),
    'o' -> new VowelProperty((MID  | BACK  | LABIAL | SHORT).toByte),
    'ó' -> new VowelProperty((MID  | BACK  | LABIAL | LONG).toByte),
    // The letters ø and ǫ (o-ogonek) always represent short vowels.
    'ø' -> new VowelProperty((MID  | FRONT | LABIAL | SHORT).toByte),
    // The letters æ and œ always represent long vowels, and do not therefore carry an accent mark.
    'œ' -> new VowelProperty((MID  | FRONT | LABIAL | LONG).toByte),
    // The letters ø and ǫ (o-ogonek) always represent short vowels.
    'ǫ' -> new VowelProperty((LOW  | BACK  | LABIAL | SHORT).toByte),
    'u' -> new VowelProperty((HIGH | BACK  | LABIAL | SHORT).toByte),
    'ú' -> new VowelProperty((HIGH | BACK  | LABIAL | LONG).toByte),
    'y' -> new VowelProperty((HIGH | FRONT | LABIAL | SHORT).toByte),
    'ý' -> new VowelProperty((HIGH | FRONT | LABIAL | LONG).toByte)
  )

  private val DIPHTONG_MAP: Map[String, Property] = Map(
    // diphtongs
    "au" -> DiphthongProperty,
    "ei" -> DiphthongProperty,
    "øy" -> DiphthongProperty
  )

  def allVowels(): Set[Char] = VOWEL_MAP.keys.toSet
  def allDiphtongs(): Set[String] = DIPHTONG_MAP.keys.toSet
}

final case class SimpleVowel(c: Char, phonemeProperty: PhonemeProperty) extends Vowel2 {

  val lengthInLetters: Int = 1

  val asString: String = c.toString

  private val LONG_VOWELS = "áæéíóœúý".toSet

  override def isFrontMutated: Boolean = "æøœyý" contains c

  override def isLong = LONG_VOWELS contains c

  //override def replaceProperty(phonemeProperty: PhonemeProperty): SimpleVowel = SimpleVowel(c, phonemeProperty)
  override def copyWithPropertyOf(phonemeProperty: PhonemeProperty) = SimpleVowel(c, phonemeProperty)

  override def toString: String = s"${c.toString}:1"
}

final case class Diphtong(vs: String, phonemeProperty: PhonemeProperty) extends Vowel2 {

  assert(vs.length == 2, "A diphtong must consist of exacltly two vowels.")

  override def isFrontMutated: Boolean = vs == "ei" || vs == "øy"

  def isLong = true

  val lengthInLetters: Int = 2

  val asString: String = vs

  override def copyWithPropertyOf(phonemeProperty: PhonemeProperty): Diphtong = Diphtong(vs, phonemeProperty)

  override def toString: String = s"$vs:2"
}