package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant.isConsonant

/**
	* Created by HyenaWarrior on 2017.10.20..
	*/
object StemTransform {

  trait Transformation {

    protected val SRC_NUCLEUS: String
    protected val DST_NUCLEUS: String

    final def apply(stemStr: String): Option[String] = transform(stemStr, SRC_NUCLEUS, DST_NUCLEUS)
    final def unapply(stemStr: String): Option[String] = transform(stemStr, DST_NUCLEUS, SRC_NUCLEUS)

    protected def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String]
  }

	object Breaking extends Transformation {

		// assume that a stem has only one syllable (?)
		val SRC_NUCLEUS: String = "e"
		val DST_NUCLEUS: String = "ja"

		override def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

			val idxOfJa =	stemStr indexOf nucleus
			if(idxOfJa == -1) return None

			val idxOfNucleusEnd = idxOfJa + nucleus.length
			val onset = stemStr.substring(0, idxOfJa)
			val coda = stemStr substring idxOfNucleusEnd

			if (!isEligible(onset, coda)) return None

			Some(onset + newNucleus + coda)
		}

		/**
		https://lrc.la.utexas.edu/eieol/norol/20#grammar_1398
			https://lrc.la.utexas.edu/eieol/norol/60#grammar_1454

			> This rule only applies to [the infinitive and present plural forms]* of verbs whose stem ends in
			a consonant cluster beginning with l or r.
			> Fracture does not occur at all if *e is preceded by v, l, or r, e.g. verða, leðr.

			* Sg 1-3 has I-umlaut, that reverses -ja- to -e- with the help of semivowel-deletion.
			  So I assume that it's applied to the whole present stem.
			*/
		private def isEligible(onset: String, coda: String): Boolean = {

			val prevCons = onset.lastOption.getOrElse(' ')
			val clusterFirstCons = coda.charAt(0)
			val clusterSecondCons = coda.charAt(1)

			val firstIsVLR = "vlr" contains prevCons
			val secondIsLR = "lr" contains clusterFirstCons
			val thirdIsCons = isConsonant(clusterSecondCons)

			!firstIsVLR && secondIsLR && thirdIsCons
		}
	}

  object JuToJo extends Transformation {

    protected val SRC_NUCLEUS: String = "jú"
    protected val DST_NUCLEUS: String = "jó"

    protected def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

      val idxOfNucleus = stemStr.indexOf(nucleus)
      val idxOfNucleusEnd = idxOfNucleus + nucleus.length

      if(isEligible(stemStr, idxOfNucleusEnd)) {

        val onset = stemStr.substring(0, idxOfNucleus)
        val coda  = stemStr.substring(idxOfNucleusEnd)

        Some(onset + newNucleus + coda)

      } else None
    }

    private def isEligible(stemStr: String, idxOfNucleusEnd: Int): Boolean = (stemStr.length > idxOfNucleusEnd) && {

      val nextLetter = stemStr.charAt(idxOfNucleusEnd)

      Consonant.isDental(nextLetter)
    }
  }

  object Raising extends Transformation {

    val SRC_NUCLEUS: String = "e"
    val DST_NUCLEUS: String = "i"

    override def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

      val idxOfE = stemStr.indexOf(nucleus)

      val coda = stemStr.substring(idxOfE + nucleus.length)

      if(coda startsWith "n") {

        val onset = stemStr.substring(0, idxOfE)

        Some(onset + newNucleus + coda)

      } else None
    }
  }
}
