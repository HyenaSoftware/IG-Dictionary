package com.hyenawarrior

import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.StaticAblaut
import com.hyenawarrior.OldNorseGrammar.grammar.{Pronoun, Word, verbs}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbTenseEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.StrongVerbStemClasses
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stemclasses.VerbStemClass._
import org.junit.Assert._
import org.junit.Test

/**
	* Created by HyenaWarrior on 2017.06.26..
	*/
class VerbTest
{
	private def convertTo(sv: StrongVerb, fp: FinitiveVerbDesc) =
	{
		StrongVerbStemClasses.convertTo(sv, sv.verbClassDesc.ablaut.asInstanceOf[StaticAblaut], fp)
	}

	private def convertTo(sv: StrongVerb, nfp: NonFinitiveVerbType) =
	{
		StrongVerbStemClasses.convertTo(sv, sv.verbClassDesc.ablaut.asInstanceOf[StaticAblaut], nfp)
	}

	@Test
	def testStrong6VerbInflection()
	{
		val vcd = verbs.getDescOfStrongVerbClassFor(VerbClassEnum.STRONG_6TH_CLASS)

		assertTrue(vcd.isDefined)

		val baseVerb = FinitiveStrongVerb("taka", vcd.head, Pronoun.PL_3_NEUT, VerbTenseEnum.PRESENT, VerbModeEnum.INDICATIVE)
		// NonFinitiveStrongVerb("fara", VerbClassEnum.STRONG_6TH_CLASS, NonFinitiveVerbType.INFINITIVE)

		//assertEquals("farið", 	convertTo(baseVerb, Right(NonFinitiveVerbType.PAST_PARTICIPLE)).get).strForm())
		assertEquals("takandi", Word(convertTo(baseVerb, NonFinitiveVerbType.PRESENT_PARTICIPLE).get).strForm())

		assertEquals("tek", 		Word(convertTo(baseVerb, 	(Pronoun.SG_1, 			INDICATIVE, PRESENT)).get).strForm())
		assertEquals("tekr", 		Word(convertTo(baseVerb, 	(Pronoun.SG_2,			INDICATIVE, PRESENT)).get).strForm())
		assertEquals("tekr", 		Word(convertTo(baseVerb, 	(Pronoun.SG_3_MASC,	INDICATIVE, PRESENT)).get).strForm())

		assertEquals("tökum", Word(convertTo(baseVerb, (Pronoun.PL_1,				INDICATIVE, PRESENT)).get).strForm())
		assertEquals("takið", Word(convertTo(baseVerb, (Pronoun.PL_2,				INDICATIVE, PRESENT)).get).strForm())
		assertEquals("taka",	Word(convertTo(baseVerb, (Pronoun.PL_3_MASC,	INDICATIVE, PRESENT)).get).strForm())

		assertEquals("tók", 		Word(convertTo(baseVerb, (Pronoun.SG_1, 			INDICATIVE, PAST)).get).strForm())
		assertEquals("tókt" /*"tókst"*/, Word(convertTo(baseVerb, (Pronoun.SG_2,		 		INDICATIVE, PAST)).get).strForm())
		assertEquals("tók", 	Word(convertTo(baseVerb, (Pronoun.SG_3_MASC,	INDICATIVE, PAST)).get).strForm())

		assertEquals("tókum", Word(convertTo(baseVerb, (Pronoun.PL_1,				INDICATIVE, PAST)).get).strForm())
		assertEquals("tókuð", Word(convertTo(baseVerb, (Pronoun.PL_2,				INDICATIVE, PAST)).get).strForm())
		assertEquals("tóku",	Word(convertTo(baseVerb, (Pronoun.PL_3_MASC, 	INDICATIVE, PAST)).get).strForm())
	}
}
