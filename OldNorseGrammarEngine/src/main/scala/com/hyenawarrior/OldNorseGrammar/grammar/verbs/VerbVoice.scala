package com.hyenawarrior.OldNorseGrammar.grammar.verbs

import com.hyenawarrior.auxiliary.EnumLike

/**
  * Created by HyenaWarrior on 2018.01.19..
  */
case class VerbVoice(name: String) {

  VerbVoice add name -> this
}

object VerbVoice extends EnumLike[String, VerbVoice] {

  val ACTIVE = VerbVoice("Active")
  val MEDIO_PASSIVE = VerbVoice("Mediopassive")
}
