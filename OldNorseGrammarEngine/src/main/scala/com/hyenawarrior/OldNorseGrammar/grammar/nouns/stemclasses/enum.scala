package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.auxiliary.enum.{EnumConst, EnumLike, EnumLookup}
/**
  * Created by HyenaWarrior on 2017.04.09..
  */
object enum {

  abstract class NounStemClassEnum extends EnumConst[NounStemClassEnum] with NounStemClass {

    override def toString: String
  }

  implicit object NounStemClassEnum extends EnumLike[NounStemClassEnum] {

    val STRONG_MASCULINE_A = StrongStemClassMascA
    val STRONG_MASCULINE_I = StrongStemClassMascI
    val STRONG_MASCULINE_U = StrongStemClassMascU
    val STRONG_MASCULINE_R = StrongStemClassMascR

    val STRONG_FEMININE_A1 = StrongStemClassFeminineA1
    val STRONG_FEMININE_A2 = StrongStemClassFeminineA2
    val STRONG_FEMININE_I = StrongStemClassFeminineI
    val STRONG_FEMININE_R = StrongStemClassFeminineR

    val STRONG_NEUTER			= StrongStemClassNeuter

    val WEAK_MASCULINE_A = WeakStemClassMascA
    val WEAK_MASCULINE_R = WeakStemClassMascR

    val WEAK_FEMININE_I = WeakStemClassFeminineI
    val WEAK_FEMININE_U = WeakStemClassFeminineU

    val WEAK_NEUTER_U		= WeakStemClassNeuter
  }

  implicit object NounStemClassEnumNameLookup extends EnumLookup[NounStemClassEnum, String] {

    override def keyOf(enumConst: NounStemClassEnum): String = enumConst.toString
  }
}
