package com.hyenawarrior.OldNorseGrammar.grammar.nominal

import com.hyenawarrior.OldNorseGrammar.grammar.{Syllables, Word}
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.AdjectiveTraits
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.Stage
import com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.calculators.Calculator
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.U_Umlaut

/**
  * Created by HyenaWarrior on 2018.09.20..
  */
object UmlautCalculator extends Calculator[AdjectiveFormType] {

  override def compute(str: String, declension: AdjectiveFormType, stage: Stage[AdjectiveFormType]) = Left {

    val optTransform = AdjectiveTraits.transformationsFor(declension)
    val inflection = AdjectiveTraits.inflectionFor(declension)

    val nonProductiveUumlaut = optTransform match {
      case Some(U_Umlaut) => true
      case _ => false
    }

    val inflectionTriggerUmlaut = U_Umlaut.triggers.map(inflection.contains(_)).fold(false)(_ || _)

    if(inflectionTriggerUmlaut || nonProductiveUumlaut) {

      val rs = U_Umlaut(str)

      Seq(rs getOrElse str)

    } else {

      Seq(str)
    }
  }

  override def reverseCompute(form: String, declension: AdjectiveFormType, stage: Stage[AdjectiveFormType]) = Left {

    // TODO: the inflection may be attached here
    // gǫmlum -> gamlum/gamlam

    val optTransform = AdjectiveTraits.transformationsFor(declension)

    val Word(sys) = Word(form)

    val possibleUmlaut = optTransform match {
      // non-productive transformation
      case Some(U_Umlaut) => Some(U_Umlaut)
      // productive U-umlaut
      case None if sys.head.nucleus == "ǫ" => Some(U_Umlaut)
      case _ => None
    }

    val newSys = possibleUmlaut match {

      case Some(U_Umlaut) => sys.zipWithIndex.map {

        case (sy, 0 | 1) => sy.replaceNucleus("a")
        case (sy, _) => sy
      }

      case _ => sys
    }

    val form2 = Syllables(newSys)

    Seq(form2)
  }

  override def shortCode: String = "UML"
}