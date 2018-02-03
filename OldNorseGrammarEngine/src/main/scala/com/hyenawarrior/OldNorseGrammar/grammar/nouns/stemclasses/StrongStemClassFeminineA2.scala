package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.Explicit_U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*
	* The a2-class has u-umlaut of an underlying a in the root, except in the genitive
	* This subclass also includes feminine nouns derived by the suffixes -ing/ung.
	* They have the suffix -u in the dative: dróttningu ‘queen’.
	*/
object StrongStemClassFeminineA2 extends StrongStemClassFeminineA
{
	def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => List(Explicit_U_Umlaut)
		case _ => List()
	}

	override def inflection(decl: (GNumber, Case)) = decl match
	{
		case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => ""
		case _ => super.inflection(decl)
	}
}
