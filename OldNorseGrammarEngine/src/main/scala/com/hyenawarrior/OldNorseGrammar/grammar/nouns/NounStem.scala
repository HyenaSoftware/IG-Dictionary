package com.hyenawarrior.OldNorseGrammar.grammar.nouns

import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.NounStemClass

/**
  * Created by HyenaWarrior on 2018.01.31..
  */
case class NounStem(rootStr: String, stemClass: NounStemClass)

object NounStem {

  def fromStrRepr(stemStr: String, stemClass: NounStemClass): NounStem = {

    val rootStr = removeThematicVowel(stemStr, stemClass)

    NounStem(rootStr, stemClass)
  }

  def removeThematicVowel(stemStr: String, stemClass: NounStemClass): String = {

    val thematicVowel = stemClass.thematicVowel.getOrElse("")

    stemStr stripSuffix thematicVowel
  }
}