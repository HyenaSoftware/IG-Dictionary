package com.hyenawarrior.oldnorsedictionary

import android.view.View
import android.widget.TextView
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.Adjective
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender.{FEMININE, MASCULINE, NEUTER}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{GNumber, Pronoun}
import com.hyenawarrior.OldNorseGrammar.grammar.morphophonology.AblautGrade
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.Noun
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.stemclasses.enum.NounStemClassEnum
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice.{ACTIVE, MEDIO_PASSIVE}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{FinitiveMood, StrongVerbClassEnum, VerbClassEnum, VerbVoice, WeakVerbClassEnum}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.stem.enum.EnumVerbStem._
import com.hyenawarrior.OldNorseGrammar.grammar.{PoSForm, Pos}

/**
  * Created by HyenaWarrior on 2017.12.03..
  */
package object modelview {

  private val NOUN_EDIT_TEXTS = Seq(
    R.id.tvNewWord_Nom_Sg -> (SINGULAR, NOMINATIVE),
    R.id.tvNewWord_Acc_Sg -> (SINGULAR, ACCUSATIVE),
    R.id.tvNewWord_Dat_Sg -> (SINGULAR, DATIVE),
    R.id.tvNewWord_Gen_Sg -> (SINGULAR, GENITIVE),

    R.id.tvNewWord_Nom_Pl -> (PLURAL, NOMINATIVE),
    R.id.tvNewWord_Acc_Pl -> (PLURAL, ACCUSATIVE),
    R.id.tvNewWord_Dat_Pl -> (PLURAL, DATIVE),
    R.id.tvNewWord_Gen_Pl -> (PLURAL, GENITIVE)
  )

  private val ADJECTIVE_TEXT_VIEWS = Seq(

    R.id.tv_masc_nom -> (MASCULINE, NOMINATIVE),
    R.id.tv_masc_acc -> (MASCULINE, ACCUSATIVE),
    R.id.tv_masc_dat -> (MASCULINE, DATIVE),
    R.id.tv_masc_gen -> (MASCULINE, GENITIVE),

    R.id.tv_fem_nom -> (FEMININE, NOMINATIVE),
    R.id.tv_fem_acc -> (FEMININE, ACCUSATIVE),
    R.id.tv_fem_dat -> (FEMININE, DATIVE),
    R.id.tv_fem_gen -> (FEMININE, GENITIVE),

    R.id.tv_neu_nom -> (NEUTER, NOMINATIVE),
    R.id.tv_neu_acc -> (NEUTER, ACCUSATIVE),
    R.id.tv_neu_dat -> (NEUTER, DATIVE),
    R.id.tv_neu_gen -> (NEUTER, GENITIVE)
  )
  
  private val VERB_NON_FINITIVE_IDS = Seq(

    (INFINITIVE, None,          None) -> R.id.tv_addword_verb_Infinitive,
    (PARTICIPLE, Some(PRESENT), None) -> R.id.tv_addword_verb_PresPart,
    (PARTICIPLE, Some(PAST),    None) -> R.id.tv_addword_verb_PastPart
  )

  private val VERB_FINITIVE_IDS = Seq(

    (PRESENT, Pronoun.SG_1) -> R.id.tv_addword_verb_PresSg1,
    (PRESENT, Pronoun.SG_2) -> R.id.tv_addword_verb_PresSg2,
    (PRESENT, Pronoun.SG_3) -> R.id.tv_addword_verb_PresSg3,

    (PRESENT, Pronoun.PL_1) -> R.id.tv_addword_verb_PresPl1,
    (PRESENT, Pronoun.PL_2) -> R.id.tv_addword_verb_PresPl2,
    (PRESENT, Pronoun.PL_3) -> R.id.tv_addword_verb_PresPl3,

    (PAST, Pronoun.SG_1) -> R.id.tv_addword_verb_PastSg1,
    (PAST, Pronoun.SG_2) -> R.id.tv_addword_verb_PastSg2,
    (PAST, Pronoun.SG_3) -> R.id.tv_addword_verb_PastSg3,

    (PAST, Pronoun.PL_1) -> R.id.tv_addword_verb_PastPl1,
    (PAST, Pronoun.PL_2) -> R.id.tv_addword_verb_PastPl2,
    (PAST, Pronoun.PL_3) -> R.id.tv_addword_verb_PastPl3
  )

  def setDeclensionsTo(noun: Noun, targetView: View): Unit = {

    val descView   = targetView.findViewById[TextView](R.id.tv_noun_class)
    val indefView = targetView.findViewById[View](R.id.frame_noun_indef)
    val defView   = targetView.findViewById[View](R.id.frame_noun_def)

    descView.setText(noun.stem.stemClass.toString)

    setDeclensionsTo(noun, indefView, isDefinite = false)
    setDeclensionsTo(noun, defView, isDefinite = true)
  }

  def setDeclensionsTo(noun: Noun, nscEnum: NounStemClassEnum, targetView: View, isDefinite: Boolean): Unit = {

    val tvNounDeclDesc = targetView.findViewById[TextView](R.id.tvNounDeclDesc)
    tvNounDeclDesc.setText(nscEnum.toString)

    setDeclensionsTo(noun, targetView, isDefinite)
  }

  def setDeclensionsTo(noun: Noun, targetView: View, isDefinite: Boolean): Unit = {

    for((id, nf) <- NOUN_EDIT_TEXTS) {

      val tvNC = targetView.findViewById[TextView](id)
      val ncTextForm = noun.nounForms.get(nf -> isDefinite).map(_.strRepr).getOrElse("...")
      tvNC.setText(ncTextForm)
    }
  }

  def setDeclensionsTo(adjective: Adjective, targetView: View, adjectiveType: AdjectiveType): Unit = {

    val tvAdjDef = targetView.findViewById[TextView](R.id.tvAdjectiveDefiniteness)
    val tvAdjKind = targetView.findViewById[TextView](R.id.tvAdjectiveKind)

    val (strKind, strDef) = adjectiveType match {

      case POSITIVE_INDEFINITE => "Positive" -> "Strong"
      case POSITIVE_DEFINITE => "Positive" -> "Weak"
      case COMPARATIVE => "Comparative" -> "Weak"
      case SUPERLATIVE_INDEFINITE => "Superlative" -> "Strong"
      case SUPERLATIVE_DEFINITE => "Superlative" -> "Weak"
    }

    tvAdjDef setText strDef
    tvAdjKind setText strKind

    setDeclensionsTo(adjective, targetView.findViewById[View](R.id.adjective_singulars), adjectiveType, SINGULAR)
    setDeclensionsTo(adjective, targetView.findViewById[View](R.id.adjective_plurals),   adjectiveType, PLURAL)
  }

  private def setDeclensionsTo(adjective: Adjective, targetView: View, adjType: AdjectiveType, number: GNumber): Unit = {

    for((id, (gender, caze)) <- ADJECTIVE_TEXT_VIEWS) {

      val tv = targetView.findViewById[TextView](id)
      val form = adjective.forms((adjType, number, gender, caze))
      tv.setText(form.strRepr)
    }
  }

  def setDeclensionsTo[K, F <: PoSForm](pos: Pos[K, F], targetView: View): Unit = pos match {

    case noun: Noun => setDeclensionsTo(noun, targetView)
    case verb: Verb => setDeclensionsTo(verb, targetView)
  }

  /**
    * Show only nonfinitives + indicative or subjunctive conjugation
    * @param v
    * @param targetView
    * @param mood
    * @param voice
    */
  def setDeclensionsTo(v: Verb, targetView: View, mood: FinitiveMood, voice: VerbVoice): Unit = v match {
    case sv @ StrongVerb(cl, ablautGrade: Map[EnumVerbStem, AblautGrade], _, _, _) =>

      setVerbConjugationDetailsTo(targetView, cl, ablautGrade)

      setInfinitiveConjugationsTo(sv, targetView, voice)
      setFinitiveConjugationTo(sv, targetView, mood, voice)

    case vw @ WeakVerb(cl, _, _, _) =>

      setVerbConjugationDetailsTo(targetView, cl)

      setInfinitiveConjugationsTo(vw, targetView, voice)
      setFinitiveConjugationTo(vw, targetView, mood, voice)
  }

  private def setVerbConjugationDetailsTo(targetView: View, cl: StrongVerbClassEnum, ablautGrade: Map[EnumVerbStem, AblautGrade]): Unit = {
    val stemName = targetView.findViewById[TextView](R.id.tv_addword_verb_stemName)
    stemName.setText(cl.name)

    val ablautDesc = Seq(PRESENT_STEM, PRETERITE_SINGULAR_STEM, PRETERITE_PLURAL_STEM, PERFECT_STEM)
      .map(ablautGrade)
      .mkString("(", " - ", ")")
      .replace("ǫ", "ö")

    // set ablaut grades
    val tv_addword_verb_AblautGrades = targetView.findViewById[TextView](R.id.tv_addword_verb_AblautGrades)
    tv_addword_verb_AblautGrades.setText(ablautDesc)
  }

  private def setVerbConjugationDetailsTo(targetView: View, cl: WeakVerbClassEnum): Unit = {

    val stemName = targetView.findViewById[TextView](R.id.tv_addword_verb_stemName)
    stemName.setText(cl.name)

    val weakVerbType = cl match {
      case VerbClassEnum.WEAK_A_STEM => "Type 2"
      case VerbClassEnum.WEAK_J_STEM => "Type 1"
      case VerbClassEnum.WEAK_I_STEM => "Type 3"
    }

    //
    val tv_addword_verb_AblautGrades = targetView.findViewById[TextView](R.id.tv_addword_verb_AblautGrades)
    tv_addword_verb_AblautGrades.setText(weakVerbType)
  }

  /**
    * Show only nonfinitives + indicative or subjunctive conjugation
    *
    * @param verb
    */
  def setDeclensionsTo(verb: Verb, rootView: View): Unit = {

    val frame_active_non = rootView.findViewById[View](R.id.frame_active_nonfinitive)
    val frame_active_ind = rootView.findViewById[View](R.id.frame_active_indicatives)
    val frame_active_subj = rootView.findViewById[View](R.id.frame_active_subjunctives)

    val frame_medpass_non = rootView.findViewById[View](R.id.frame_mediopassive_nonfinitive)
    val frame_medpass_ind = rootView.findViewById[View](R.id.frame_mediopassive_indicative)
    val frame_medpass_subj = rootView.findViewById[View](R.id.frame_mediopassive_subjunctive)

    setInfinitiveConjugationsTo(verb, frame_active_non, ACTIVE)
    setFinitiveConjugationTo(verb, frame_active_ind, INDICATIVE, ACTIVE)
    setFinitiveConjugationTo(verb, frame_active_subj, SUBJUNCTIVE, ACTIVE)

    setInfinitiveConjugationsTo(verb, frame_medpass_non, MEDIO_PASSIVE)
    setFinitiveConjugationTo(verb, frame_medpass_ind, INDICATIVE, MEDIO_PASSIVE)
    setFinitiveConjugationTo(verb, frame_medpass_subj, SUBJUNCTIVE, MEDIO_PASSIVE)
  }

  def setInfinitiveConjugationsTo(v: Verb, targetView: View, voice: VerbVoice): Unit = {

    for ((vt, id) <- VERB_NON_FINITIVE_IDS) {

      val wt = vt match {

        case (m, oT, oP) => (m, voice, oT, oP)
      }

      val f = v.forms(wt)
      val tv = targetView.findViewById[TextView](id)
      tv.setText(f.strRepr.replace("ǫ", "ö"))
    }
  }

  def setFinitiveConjugationTo(sv: Verb, targetView: View, mood: FinitiveMood, voice: VerbVoice): Unit = {

    for ((vt, id) <- VERB_FINITIVE_IDS) {

      val wt = vt match {

        case (t, p) => (mood, voice, Some(t), Some(p))
      }

      val f = sv.forms(wt)
      val tv = targetView.findViewById[TextView](id)
      tv.setText(f.strRepr.replace("ǫ", "ö"))
    }
  }
}
