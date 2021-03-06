package com.hyenawarrior.OldNorseGrammar.grammar.morphophonology

import com.hyenawarrior.OldNorseGrammar.grammar.{Syllable, Syllables}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem._

/**
  * Created by HyenaWarrior on 2017.04.25..
  */
case class StaticAblaut(presentAblautGrade: AblautGrade, preteriteSingularAblautGrade: AblautGrade
                        , preteritePluralAblautGrade: AblautGrade, perfectAblautGrade: AblautGrade) {

  private val VOWELS = Map(
    PRESENT_STEM						-> presentAblautGrade,
    PRETERITE_SINGULAR_STEM	-> preteriteSingularAblautGrade,
    PRETERITE_PLURAL_STEM	 	-> preteritePluralAblautGrade,
    PERFECT_STEM						-> perfectAblautGrade
  )

  def grades(enumVerbStem: EnumVerbStem): AblautGrade = VOWELS(enumVerbStem)

  override def toString: String	= List(presentAblautGrade, preteriteSingularAblautGrade, preteritePluralAblautGrade, perfectAblautGrade)
    .mkString("(", " - ", ")")
}

object Ablaut {

  def transform(str: String, srcAblaut: AblautGrade, dstAblaut: AblautGrade): Option[String] = {

    val where = str.indexOf(srcAblaut.rootVowel)

    if (where != -1) {

      val length = srcAblaut.rootVowel.length

      val prefixStr = str.substring(0, where)
      val suffixStr = str.substring(where + length)

      Some(s"$prefixStr${dstAblaut.rootVowel}$suffixStr")

    } else None
  }

  /**
    *
    * @param rawStr
    * @return
    * @throws RuntimeException if the input string doesn't have vowel
    */
  def getAblautGradeFrom(rawStr: String): AblautGrade = {

    val Syllables(syllables) = rawStr

    val firstSy = syllables.head

    val Syllable(onset, nucleus, _, true, _) = firstSy

    if (nucleus.nonEmpty) AblautGrade(onset.lastOption match {

      case Some('j') =>	"j" + nucleus
      case _ => nucleus

    }) else {

      throw new RuntimeException(String.format("'%s' syllable of '%s' doesn't have any vowel.", firstSy.letters, rawStr))
    }
  }

  def extractAblautFrom(verb: Map[EnumVerbStem, Seq[String]]): Option[StaticAblaut] = {

    val setPsSV		= verb.getOrElse(PRESENT_STEM, 						Seq()).map(s => getAblautGradeFrom(s)).toSet
    val setPtSgSV	= verb.getOrElse(PRETERITE_SINGULAR_STEM, Seq()).map(s => getAblautGradeFrom(s)).toSet
    val setPtPlSV	= verb.getOrElse(PRETERITE_PLURAL_STEM,		Seq()).map(s => getAblautGradeFrom(s)).toSet
    val setPfSV		= verb.getOrElse(PERFECT_STEM,						Seq()).map(s => getAblautGradeFrom(s)).toSet

    val psSV   = if(setPsSV.size == 1) setPsSV.headOption else None
    val ptSgSV = if(setPtSgSV.size == 1) setPtSgSV.headOption else None
    val ptPlSV = if(setPtPlSV.size == 1) setPtPlSV.headOption else None
    val pfSV   = if(setPfSV.size == 1) setPfSV.headOption else None

    // complete missing ablaut-grades
    val optFixedPsSv		= psSV		orElse pfSV 	orElse ptSgSV	orElse ptPlSV
    val optFixedPtSgSv	= ptSgSV	orElse ptPlSV orElse pfSV		orElse psSV
    val optFixedPtPlSv	= ptPlSV 	orElse ptSgSV orElse pfSV	 	orElse psSV
    val optFixedPfSv		= pfSV		orElse psSV 	orElse ptSgSV orElse ptPlSV

    (optFixedPsSv, optFixedPtSgSv, optFixedPtPlSv, optFixedPfSv) match  {

      case (Some(fixedPsSv), Some(fixedPtSgSv), Some(fixedPtPlSv), Some(fixedPfSv)) =>
        Some(StaticAblaut(fixedPsSv, fixedPtSgSv, fixedPtPlSv, fixedPfSv))
      case _ =>  None
    }
  }
}