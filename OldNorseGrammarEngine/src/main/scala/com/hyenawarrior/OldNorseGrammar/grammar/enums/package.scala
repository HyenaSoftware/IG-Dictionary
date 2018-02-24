package com.hyenawarrior.OldNorseGrammar.grammar

import com.hyenawarrior.auxiliary.enum.{EnumConst, EnumLike}


/**
  * Created by HyenaWarrior on 2018.02.24..
  */
package object enums {

  final case class Case(name: String) extends EnumConst[Case] {

    override def toString = name + " case"
  }

  case class Gender(name: String) extends EnumConst[Gender]

  final case class GNumber(str: String) extends EnumConst[GNumber] {

    override def toString = str
  }

  case class Pronoun(number: GNumber, person: Int) extends EnumConst[Pronoun] {

    override def toString: String = s"$ordinal person $number"

    private def ordinal = person match {

      case 1 => "1st"
      case 2 => "2nd"
      case 3 => "3rd"
    }
  }

  implicit object Case extends EnumLike[Case] {

    val NOMINATIVE	= Case("Nominative")
    val ACCUSATIVE	= Case("Accusative")
    val DATIVE 			= Case("Dative")
    val GENITIVE		= Case("Genitive")
  }

  implicit object GNumber extends EnumLike[GNumber] {

    val SINGULAR = GNumber("Singular")
    val DUAL = GNumber("Dual")
    val PLURAL = GNumber("Plural")

    def conventionalValues = List(SINGULAR, PLURAL)
  }

  implicit object Pronoun extends EnumLike[Pronoun] {

    val SG_1 = Pronoun(GNumber.SINGULAR, 1)
    val SG_2 = Pronoun(GNumber.SINGULAR, 2)
    val SG_3 = Pronoun(GNumber.SINGULAR, 3)

    val PL_1 = Pronoun(GNumber.PLURAL, 1)
    val PL_2 = Pronoun(GNumber.PLURAL, 2)
    val PL_3 = Pronoun(GNumber.PLURAL, 3)
  }

  implicit object Gender extends EnumLike[Gender] {

    val MASCULINE = Gender("Masculine")
    val FEMININE = Gender("Feminine")
    val NEUTER = Gender("Neuter")
  }
}
