package com.hyenawarrior.OldNorseGrammar.grammar.phonology

/**
  * Created by HyenaWarrior on 2018.09.21..
  */
trait MorphemeProperty

object MorphemeProperty {

  object Composite          extends MorphemeProperty { override def toString: String = "Composite" }
  object Stem               extends MorphemeProperty { override def toString: String = "Stem" }
  object StressedSyllable   extends MorphemeProperty { override def toString: String = "StressedSyllable" }
  object UnStressedSyllable extends MorphemeProperty
  object StemAndSuffix      extends MorphemeProperty { override def toString: String = "Whole" }
  object Suffix             extends MorphemeProperty { override def toString: String = "Suffix" }
  object VoicedConsonantContext     extends MorphemeProperty
  object VoicelessConsonantContext  extends MorphemeProperty
}