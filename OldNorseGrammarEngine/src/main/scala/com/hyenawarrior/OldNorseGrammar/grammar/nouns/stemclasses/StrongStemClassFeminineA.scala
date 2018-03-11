package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.NounType
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.07.28..
  */
abstract class StrongStemClassFeminineA extends NounStemClassEnum with FeminineNoun
{
  override def thematicVowel: Option[String] = Some("a")

  override def inflection(decl: NounType) = decl match
  {
    case (SINGULAR, NOMINATIVE | ACCUSATIVE | DATIVE) => ???
    case (SINGULAR, GENITIVE)		=> "ar"

    case (PLURAL, NOMINATIVE | ACCUSATIVE)		=> "ar"
    case (PLURAL, DATIVE)				=> "um"
    case (PLURAL, GENITIVE)			=> "a"
  }
}
