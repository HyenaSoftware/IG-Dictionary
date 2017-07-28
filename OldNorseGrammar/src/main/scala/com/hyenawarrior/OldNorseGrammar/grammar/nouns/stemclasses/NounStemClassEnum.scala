package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.auxiliary.EnumLike

/**
	* Created by HyenaWarrior on 2017.04.09..
	*/
final case class NounStemClassEnum(name: String, nounStemClass: NounStemClass)
{
	NounStemClassEnum.add(name -> this)

	override def toString = name
}

object NounStemClassEnum extends EnumLike[String, NounStemClassEnum]
{
	val STRONG_MASCULINE_A = NounStemClassEnum("Strong Masc. A-class", StrongStemClassMascA)
	val STRONG_MASCULINE_I = NounStemClassEnum("Strong Masc. I-class", StrongStemClassMascI)
	val STRONG_MASCULINE_U = NounStemClassEnum("Strong Masc. U-class", StrongStemClassMascU)
	val STRONG_MASCULINE_R = NounStemClassEnum("Strong Masc. R-class", StrongStemClassMascR)

	val STRONG_FEMININE_A1 = NounStemClassEnum("Strong Fem. A1-class", StrongStemClassFeminineA1)
	val STRONG_FEMININE_A2 = NounStemClassEnum("Strong Fem. A2-class", StrongStemClassFeminineA2)
	val STRONG_FEMININE_I = NounStemClassEnum("Strong Fem. I-class", StrongStemClassFeminineI)
	val STRONG_FEMININE_R = NounStemClassEnum("Strong Fem. R-class", StrongStemClassFeminineR)

	val STRONG_NEUTER			= NounStemClassEnum("Strong Neuter", StrongStemClassNeuter)

	val WEAK_MASCULINE_A = NounStemClassEnum("Weak Masc. A-class", WeakStemClassMascA)
	val WEAK_MASCULINE_R = NounStemClassEnum("Weak Masc. R-class", WeakStemClassMascR)

	val WEAK_FEMININE_I = NounStemClassEnum("Weak Fem. I-class", WeakStemClassFeminineI)
	val WEAK_FEMININE_U = NounStemClassEnum("Weak Fem. U-class", WeakStemClassFeminineU)

	val WEAK_NEUTER_U		= NounStemClassEnum("Weak Neuter", WeakStemClassNeuter)
}