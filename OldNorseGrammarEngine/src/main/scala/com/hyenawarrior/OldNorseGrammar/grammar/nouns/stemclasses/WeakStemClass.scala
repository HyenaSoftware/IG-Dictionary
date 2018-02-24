package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}

/**
	* Created by HyenaWarrior on 2017.07.28..
	*/
class WeakStemClass extends NounStemClass
{
	def transformationsFor(decl: (GNumber, Case)) =  decl match
	{
		case _ 	=> List(U_Umlaut)
	}

	override def inflection(decl: (GNumber, Case)): String = decl match
	{
		case (PLURAL, DATIVE)										=> "um"
		case (PLURAL, GENITIVE)									=> "a"
	}
}
