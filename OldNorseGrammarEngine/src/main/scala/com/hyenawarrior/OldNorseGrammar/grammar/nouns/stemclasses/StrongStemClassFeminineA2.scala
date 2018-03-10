package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.nouns._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*
	* The a2-class has u-umlaut of an underlying a in the root, except in the genitive
	* This subclass also includes feminine nouns derived by the suffixes -ing/ung.
	* They have the suffix -u in the dative: dróttningu ‘queen’.
	*/
object StrongStemClassFeminineA2 extends StrongStemClassFeminineA
{
	override def toString = "Strong feminine A2-class"

	override def transformationFor(decl: NounType) = decl match {

		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => Some(U_Umlaut)
		case _ => None
	}

	override def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => ""
		case _ => super.inflection(decl)
	}
}
