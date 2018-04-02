package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.PLURAL
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass
import com.hyenawarrior.OldNorseGrammar.grammar.phonology.Vowel.isVowel

/**
  * Created by HyenaWarrior on 2018.03.26..
  */
object CliticArticle {

  def apply(nounForm: NounForm, stemClass: NounStemClass): NounForm = {

    val NounForm(strRepr, declension, _) = nounForm

    val article = stemClass cliticFor declension

    // do we need to take care of diphtongs in articles? - probably not
    val articleIsBisyllabic = article.count(isVowel) > 1

    val strReprNoDatPlM = if(declension == (PLURAL, Case.DATIVE)) strRepr.stripSuffix("m") else strRepr

    // hesta: 1 == 5-1
    // á:     0 == 1-1
    // TODO: should it work for diphtongs?
    // sau:   1 == 3-1
    val rootEndingWithVowel = strRepr.indexWhere(isVowel) == strRepr.length - 1
    val nounEndingWithVowel = isVowel(strReprNoDatPlM.last)
    val pluralSuffixIsR = (declension._1 == PLURAL) && (strRepr.last == 'r')

    // (!a && b) || c
    val shouldDropTheI = {

      if(nounEndingWithVowel && (!rootEndingWithVowel || articleIsBisyllabic)) true

      else if(pluralSuffixIsR) true

      else false
    }

    val correctedArticle = if(shouldDropTheI) article.tail else article

    NounForm(strReprNoDatPlM + correctedArticle, declension, isDefinite = true)
  }

  def removeArticle(nounForm: NounForm, stemClass: NounStemClass): Option[NounForm] = if(nounForm.isDefinite) Some {

    val NounForm(strRepr, declension, _) = nounForm

    val article = stemClass cliticFor declension

    val articleLessStr = article.map(_.toString).foldRight(strRepr){ case (ac, word) => word stripSuffix ac }

    val articleLessStr2 = articleLessStr + (declension match {

      case (PLURAL, DATIVE) => "m"
      case _ => ""
    })

    NounForm(articleLessStr2, declension, isDefinite = false)

  } else None
}
