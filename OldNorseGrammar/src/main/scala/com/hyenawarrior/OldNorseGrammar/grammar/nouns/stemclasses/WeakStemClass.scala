package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses
import com.hyenawarrior.OldNorseGrammar.grammar.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
class WeakStemClass extends NounStemClass
{
	override def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		case _ 	=> List(U_Umlaut)
	}

	override protected def inflection(decl: (GNumber, Case)): String = decl match
	{
		case (PLURAL, DATIVE)										=> "um"
		case (PLURAL, GENITIVE)									=> "a"
	}
}
