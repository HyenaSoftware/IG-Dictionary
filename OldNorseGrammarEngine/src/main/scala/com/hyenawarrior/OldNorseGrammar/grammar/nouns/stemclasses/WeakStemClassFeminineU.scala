package com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses

import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum

/**
  * Created by HyenaWarrior on 2017.04.09..
  */
object WeakStemClassFeminineU extends NounStemClassEnum with FeminineNoun {

  override def toString = "Weak feminine U-class"

  override def inflection(decl: (GNumber, Case)) = decl match	{

    case (SINGULAR, NOMINATIVE)											=> "a"
    case (SINGULAR, ACCUSATIVE | DATIVE | GENITIVE)	=> "u"

    case (PLURAL, NOMINATIVE | ACCUSATIVE)	=> "ur"
    case (PLURAL, DATIVE)										=> "um"
    case (PLURAL, GENITIVE)									=> "na"
  }
}
