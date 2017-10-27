package com.hyenawarrior

import org.junit.Test
import org.junit.Assert.{assertEquals, assertNotSame, assertSame}
import com.hyenawarrior.OldNorseGrammar.grammar.soundex.soundexCodeOf

/**
	* Created by HyenaWarrior on 2017.06.21..
	*/
class SoundexTest
{
	// countOf(soundexKeys) * 5 <= counfOf(formsOfOneWord)
	val MAXIMUM_ALLOWED_KEY_DIVERSITY = 5

	val transform: String => String = soundexCodeOf

	@Test
	def testDiversity(): Unit =
	{
		compareDiversiveSoundexCode(
			Array("ríða", "flúið", "verða", "úlfr")
		)
	}

	@Test
	def testStrongVerbClass1(): Unit =
	{
		checkSoundexQuality(
			Array("ríða","ríðið", "ríðandi",
						"ríð",		"ríðr", 	"ríðr", "ríðum","ríðið","ríða",
						"reið",	"reiðst", "reið", "riðum","riðuð","riðu",
						"ríði",	"ríðir", 	"ríði", "ríðum","ríðið","ríði",
						"riði",	"riðir", 	"riði", "riðum","riðuð","riðu")
		)
	}

	@Test
	def testStrongVerbClass2(): Unit =
	{
		checkSoundexQuality(Array("flúið","flýjandi",
			"flý",		"flýrð",	"flýr", 	"flýjum",	"flýið",	"flýja",
			"flúð",	"flúðir",	"flúði",	"flúðum",	"flúðuð",	"flúðu",
			"flýi",	"flýir",	"flýi",		"flýjum",	"flýið",	"flýi",
			"flýði",	"flýðir",	"flýði",	"flýðum",	"flýðuð",	"flýðu")
		)
	}

	@Test
	def testStrongVerbClass3(): Unit =
	{
		checkSoundexQuality(Array(
			"verða",	"orðið", 	"verðandi",
			"verð",		"verðr",	"verðr",	"verðum", "verðið", "verða",
			"varð",		"varðst",	"varð",		"urðum",	"urðuð",	"urðu",
			"verði", 	"verðir",	"verði",	"verðum",	"verðið",	"verði",
			"yrði",		"yrðir",	"yrði",		"yrðum",	"yrðuð",	"yrðu")
		)
	}

	@Test
	def testNounStrongAMasc(): Unit =
	{
		checkSoundexQuality(Array(
			"úlfr",			"úlf",		"úlfi",			"úlfs",
			"úlfrinn",	"úlfinn",	"úlfinum",	"úlfsins",
			"úlfar",		"úlfa",		"úlfum",		"úlfa",
			"úlfarnir",	"úlfana",	"úlfunum",	"úlfanna")
		)
	}

	@Test
	def testNounStrongAFem(): Unit =
	{
		checkSoundexQuality(Array(
			"á",		"á",		"á",		"ár",
			"áin",	"ána",	"ánni",	"árinnar",
			"á",		"ár",		"ám",		"á",
			"árnar","árnar","ánum",	"ánna")
		)
	}

	@Test
	def testNounStrongARasc(): Unit =
	{
		checkSoundexQuality(Array(
			"maðr",			"mann",		"manni",		"manns",
			"maðrinn",	"mannin",	"manninum",	"mannsins",
			"menn",			"menn",		"mǫnnum",		"manna",
			"menninir",	"mennina","mǫnnunum",	"mannanna")
		)
	}

	@Test
	def testWeakVerbAStemWithConsonantAssimilation(): Unit =
	{
		// kalla - weak 2 verb
		checkSoundexQuality(Array(

			"kalla",		"kallandi",	"kallaðr",
			// ind
			"kalla",		"kallar",		"kallar",
			"kǫllum",		"kallið",		"kalla",
			// ind-pret
			"kallaða",	"kallaðir",	"kallaði",
			"kǫlluðum",	"kǫlluðuð",	"kǫlluðu",
			// subj
			"kalla",		"kallir",		"kallir",
			"kallim",		"kallið",		"kalli",
			// subj-pret
			"kallaða",	"kallaðir",	"kallaði",
			"kallaðim",	"kallaðið",	"kallaði"
		))
	}

	def compareSoundexCode(forms: Array[String]): Unit =
	{
		val allForms = forms.map(x => transform(x) -> x)
			.groupBy{ case(k, _) => k }
			.mapValues(_.map(_._2))	// eliminate key duplication
		  .map
			{
				case (k, v) => println(s"$k -> ${v.mkString("(", ", ", ")")}"); k
			}

		assertEquals(s"${allForms} must have only one element", 1, allForms.size)
	}

	def checkSoundexQuality(forms: Array[String]): Unit =
	{
		val countOfDistinctForms = forms.toSet.size
		val allForms = forms.map(x => transform(x) -> x)
			.groupBy{ case(k, _) => k }
		  .mapValues(_.map(_._2))

		for((k,vs) <- allForms)
		{
			println(s"$k -> ${vs.mkString("(", ", ", ")")}")
		}

		println(s" assert(${allForms.keys.size} * $MAXIMUM_ALLOWED_KEY_DIVERSITY <= $countOfDistinctForms)")

		assert(allForms.keySet.size * MAXIMUM_ALLOWED_KEY_DIVERSITY <= countOfDistinctForms)
	}

	def compareDiversiveSoundexCode(forms: Array[String]): Unit =
	{
		val allForms = forms.map(transform).toSet

		assertEquals(forms.toSet.size, allForms.size)
	}
}
