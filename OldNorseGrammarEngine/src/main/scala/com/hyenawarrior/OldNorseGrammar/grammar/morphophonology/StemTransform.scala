package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Consonant

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

  object EToJa extends Transformation {

		// assume that a stem has only one syllable (?)
    val SRC_NUCLEUS: String = "e"
    val DST_NUCLEUS: String = "ja"

		override def transform(stemStr: String, nucleus: String, newNucleus: String): Option[String] = {

			val idxOfJa = stemStr.indexOf(nucleus)
			val idxOfNucleusEnd = idxOfJa + nucleus.length

			if (isEligible(stemStr, idxOfNucleusEnd)) {

				val onset = stemStr.substring(0, idxOfJa)
				val coda = stemStr.substring(idxOfNucleusEnd)

				Some(onset + newNucleus + coda)

			} else None
		}

		private def isEligible(stemStr: String, idxOfNucleusEnd: Int): Boolean = (stemStr.length > idxOfNucleusEnd + 1) && {

			val clusterFirstCons = stemStr.charAt(idxOfNucleusEnd)
			val clusterSecondCons = stemStr.charAt(idxOfNucleusEnd + 1)

			val firstIsLR = "lr" contains clusterFirstCons
			val secondIsCons = Consonant.isConsonant(clusterSecondCons)

			firstIsLR && secondIsCons
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
}
