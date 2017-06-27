package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Word}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.06.26..
	*/
class VerbTest
{
	@Test
	def testStrong6VerbInflection()
	{
		val baseVerb = FinitiveStrongVerb("taka", VerbClassEnum.STRONG_6TH_CLASS, Pronoun.PL_3_NEUT, VerbTenseEnum.PRESENT)
		// NonFinitiveStrongVerb("fara", VerbClassEnum.STRONG_6TH_CLASS, NonFinitiveVerbType.INFINITIVE)

		//assertEquals("farið", 	convertTo(baseVerb, Right(NonFinitiveVerbType.PAST_PARTICIPLE)).get).strForm())
		assertEquals("takandi", Word(convertTo(baseVerb, Right(NonFinitiveVerbType.PRESENT_PARTICIPLE)).get).strForm())

		assertEquals("tek", 		Word(convertTo(baseVerb, 	Left(Pronoun.SG_1, 			INDICATIVE, PRESENT)).get).strForm())
		assertEquals("tekr", 		Word(convertTo(baseVerb, 	Left(Pronoun.SG_2,			INDICATIVE, PRESENT)).get).strForm())
		assertEquals("tekr", 		Word(convertTo(baseVerb, 	Left(Pronoun.SG_3_MASC,	INDICATIVE, PRESENT)).get).strForm())

		assertEquals("tökum", Word(convertTo(baseVerb, Left(Pronoun.PL_1,				INDICATIVE, PRESENT)).get).strForm())
		assertEquals("takið", Word(convertTo(baseVerb, Left(Pronoun.PL_2,				INDICATIVE, PRESENT)).get).strForm())
		assertEquals("taka",	Word(convertTo(baseVerb, Left(Pronoun.PL_3_MASC,	INDICATIVE, PRESENT)).get).strForm())

		assertEquals("tók", 		Word(convertTo(baseVerb, Left(Pronoun.SG_1, 			INDICATIVE, PAST)).get).strForm())
		assertEquals("tókt" /*"tókst"*/, Word(convertTo(baseVerb, Left(Pronoun.SG_2,		 		INDICATIVE, PAST)).get).strForm())
		assertEquals("tók", 	Word(convertTo(baseVerb, Left(Pronoun.SG_3_MASC,	INDICATIVE, PAST)).get).strForm())

		assertEquals("tókum", Word(convertTo(baseVerb, Left(Pronoun.PL_1,				INDICATIVE, PAST)).get).strForm())
		assertEquals("tókuð", Word(convertTo(baseVerb, Left(Pronoun.PL_2,				INDICATIVE, PAST)).get).strForm())
		assertEquals("tóku",	Word(convertTo(baseVerb, Left(Pronoun.PL_3_MASC, 	INDICATIVE, PAST)).get).strForm())
	}
}
