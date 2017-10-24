package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant

/**
	* Created by HyenaWarrior on 2017.10.20..
	*/
object StemTransform {

  trait Transformation {

    protected val SRC_NUCLEUS: String
    protected val DST_NUCLEUS: String

    def apply(stemStr: String): String = transform(stemStr, SRC_NUCLEUS, DST_NUCLEUS)
    def unapply(stemStr: String): Option[String] = Some(transform(stemStr, DST_NUCLEUS, SRC_NUCLEUS))

    protected def transform(stemStr: String, nucleus: String, newNucleus: String): String
  }

  object EToJa extends Transformation {

		// assume that a stem has only one syllable (?)
    val SRC_NUCLEUS: String = "e"
    val DST_NUCLEUS: String = "ja"

		protected def transform(stemStr: String, nucleus: String, newNucleus: String): String = {

			val idxOfJa = stemStr.indexOf(nucleus)
			val idxOfNucleusEnd = idxOfJa + nucleus.length

			if (isEligible(stemStr, idxOfNucleusEnd)) {

				val onset = stemStr.substring(0, idxOfJa)
				val coda = stemStr.substring(idxOfNucleusEnd)

				onset + newNucleus + coda

			} else stemStr
		}

		private def isEligible(stemStr: String, idxOfNucleusEnd: Int): Boolean = (stemStr.length > idxOfNucleusEnd + 1) && {

			val clusterFirstCons = stemStr.charAt(idxOfNucleusEnd)
			val clusterSecondCons = stemStr.charAt(idxOfNucleusEnd + 1)

			val firstIsLR = "lr" contains clusterFirstCons
			val secondIsCons = Consonant.isConsonant(clusterSecondCons)

			firstIsLR && secondIsCons
		}
	}
}
