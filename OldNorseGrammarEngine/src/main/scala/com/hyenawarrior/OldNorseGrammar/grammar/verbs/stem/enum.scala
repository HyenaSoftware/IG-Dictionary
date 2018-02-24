package com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem

import com.hyenawarrior.auxiliary.enum.{EnumConst, EnumLike}

/**
  * Created by HyenaWarrior on 2018.02.24..
  */
object enum {

  case class EnumVerbStem(name: String) extends EnumConst[EnumVerbStem]

  implicit object EnumVerbStem extends EnumLike[EnumVerbStem] {

    val PRESENT_STEM							= EnumVerbStem("Present")
    val PRETERITE_SINGULAR_STEM		= EnumVerbStem("Preterite Singular")
    val PRETERITE_PLURAL_STEM			= EnumVerbStem("Preterite Plural")
    val PERFECT_STEM							= EnumVerbStem("Perfect")
  }
}
