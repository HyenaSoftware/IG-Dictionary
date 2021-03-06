package com.hyenawarrior.oldnorsedictionary

import android.view.View
import android.widget.TextView
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.Adjective
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber.{unapply => _, _}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender.{FEMININE, MASCULINE, NEUTER}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber, Gender, Pronoun}
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

  private val STRONG_SINGULAR_ADJECTIVE_TEXT_VIEWS = Seq(

    R.id.tv_adj_strong_masc_nom -> (MASCULINE, NOMINATIVE),
    R.id.tv_adj_strong_masc_acc -> (MASCULINE, ACCUSATIVE),
    R.id.tv_adj_strong_masc_gen -> (MASCULINE, GENITIVE),
    R.id.tv_adj_strong_masc_dat -> (MASCULINE, DATIVE),

    R.id.tv_adj_strong_fem_nom -> (FEMININE, NOMINATIVE),
    R.id.tv_adj_strong_fem_acc -> (FEMININE, ACCUSATIVE),
    R.id.tv_adj_strong_fem_gen -> (FEMININE, GENITIVE),
    R.id.tv_adj_strong_fem_dat -> (FEMININE, DATIVE),

    R.id.tv_adj_strong_neu_nom_acc -> (NEUTER, NOMINATIVE),
    R.id.tv_adj_strong_neu_gen -> (NEUTER, GENITIVE),
    R.id.tv_adj_strong_neu_dat -> (NEUTER, DATIVE)
  )

  private val STRONG_PLURAL_ADJECTIVE_TEXT_VIEWS = Seq(

    R.id.tv_adj_strong_pl_masc_nom -> (MASCULINE, NOMINATIVE),
    R.id.tv_adj_strong_pl_masc_acc -> (MASCULINE, ACCUSATIVE),
    R.id.tv_adj_strong_pl_masc_fem_neu_gen -> (FEMININE, GENITIVE),
    R.id.tv_adj_strong_pl_masc_fem_neu_dat -> (FEMININE, DATIVE),

    R.id.tv_adj_strong_pl_fem_nom_acc -> (FEMININE, NOMINATIVE),

    R.id.tv_adj_strong_pl_neu_nom_acc -> (NEUTER, NOMINATIVE)
  )

  private val WEAK_SINGULAR_ADJECTIVE_TEXT_VIEWS = Seq(

    R.id.tv_adj_weak_sg_masc_nom      -> (MASCULINE, NOMINATIVE),
    R.id.tv_adj_weak_sg_masc_acc_gen_dat  -> (MASCULINE, ACCUSATIVE),

    R.id.tv_adj_weak_sg_fem_nom     -> (FEMININE, NOMINATIVE),
    R.id.tv_adj_weak_sg_fem_acc_gen_dat -> (FEMININE, ACCUSATIVE),

    R.id.tv_adj_weak_sg_neu_nom_acc_gen_dat -> (NEUTER, NOMINATIVE)
  )

  private val WEAK_PLURAL_ADJECTIVE_TEXT_VIEWS = Seq(

    R.id.tv_adj_weak_pl_nom_acc_gen -> (FEMININE, NOMINATIVE),
    R.id.tv_adj_weak_pl_dat         -> (FEMININE, DATIVE)
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

  def setDeclensionsTo(adjective: Adjective, targetView: View): Unit = {

    // 1
    val vwPosIndef = targetView.findViewById[View](R.id.frame_positive_indefinite)

    setAdjectiveDeclensionsTo(adjective, vwPosIndef, POSITIVE_INDEFINITE, SINGULAR, STRONG_SINGULAR_ADJECTIVE_TEXT_VIEWS)
    setAdjectiveDeclensionsTo(adjective, vwPosIndef, POSITIVE_INDEFINITE, PLURAL,   STRONG_PLURAL_ADJECTIVE_TEXT_VIEWS)

    // 2
    val vwPosDef = targetView.findViewById[View](R.id.frame_positive_definite)

    setAdjectiveDeclensionsTo(adjective, vwPosDef, POSITIVE_DEFINITE, SINGULAR, WEAK_SINGULAR_ADJECTIVE_TEXT_VIEWS)
    setAdjectiveDeclensionsTo(adjective, vwPosDef, POSITIVE_DEFINITE, PLURAL, WEAK_PLURAL_ADJECTIVE_TEXT_VIEWS)

    // 3
    val vwComp = targetView.findViewById[View](R.id.frame_comparative)

    setAdjectiveDeclensionsTo(adjective, vwComp, COMPARATIVE, SINGULAR, WEAK_SINGULAR_ADJECTIVE_TEXT_VIEWS)
    setAdjectiveDeclensionsTo(adjective, vwComp, COMPARATIVE, PLURAL, WEAK_PLURAL_ADJECTIVE_TEXT_VIEWS)

    // 4
    val vwSupIndefFrame = targetView.findViewById[View](R.id.frame_superlative_indefinite)

    setAdjectiveDeclensionsTo(adjective, vwSupIndefFrame, SUPERLATIVE_INDEFINITE, SINGULAR, STRONG_SINGULAR_ADJECTIVE_TEXT_VIEWS)
    setAdjectiveDeclensionsTo(adjective, vwSupIndefFrame, SUPERLATIVE_INDEFINITE, PLURAL,   STRONG_PLURAL_ADJECTIVE_TEXT_VIEWS)

    // 5
    val vwSupDefFrame = targetView.findViewById[View](R.id.frame_superlative_definite)

    setAdjectiveDeclensionsTo(adjective, vwSupDefFrame, SUPERLATIVE_DEFINITE, SINGULAR, WEAK_SINGULAR_ADJECTIVE_TEXT_VIEWS)
    setAdjectiveDeclensionsTo(adjective, vwSupDefFrame, SUPERLATIVE_DEFINITE, PLURAL, WEAK_PLURAL_ADJECTIVE_TEXT_VIEWS)
  }

  def setStrongDeclensionsTo(adjective: Adjective, targetView: View, adjectiveType: AdjectiveType): Unit = {

    val tvAdjKind = targetView.findViewById[TextView](R.id.tvAdjectiveKind)

    val strKind = adjectiveType match {

      case POSITIVE_INDEFINITE => "Positive"
      case SUPERLATIVE_INDEFINITE => "Superlative"
    }

    tvAdjKind setText strKind

    setAdjectiveDeclensionsTo(adjective, targetView, adjectiveType, SINGULAR, STRONG_SINGULAR_ADJECTIVE_TEXT_VIEWS)
    setAdjectiveDeclensionsTo(adjective, targetView, adjectiveType, PLURAL,   STRONG_PLURAL_ADJECTIVE_TEXT_VIEWS)
  }

  def setWeakDeclensionsTo(adjective: Adjective, targetView: View, adjectiveType: AdjectiveType): Unit = {

    val tvAdjKind = targetView.findViewById[TextView](R.id.tvAdjectiveKind)

    tvAdjKind setText (adjectiveType match {

      case POSITIVE_DEFINITE => "Positive"
      case COMPARATIVE => "Comparative"
      case SUPERLATIVE_DEFINITE => "Superlative"
    })

    setAdjectiveDeclensionsTo(adjective, targetView, adjectiveType, SINGULAR, WEAK_SINGULAR_ADJECTIVE_TEXT_VIEWS)
    setAdjectiveDeclensionsTo(adjective, targetView, adjectiveType, PLURAL, WEAK_PLURAL_ADJECTIVE_TEXT_VIEWS)
  }

  private def setAdjectiveDeclensionsTo(adjective: Adjective, targetView: View, adjType: AdjectiveType,
                                        number: GNumber, mapping: Seq[(Int, (Gender, Case))]): Unit = {

    for((id, (gender, caze)) <- mapping) {

      val tv = targetView.findViewById[TextView](id)
      val formType = AdjectiveFormType(adjType, number, gender, caze)
      val strForm = adjective.forms.get(formType).map(_.strRepr).getOrElse("-")
      tv.setText(strForm)
    }
  }

  def setDeclensionsTo[K, F <: PoSForm](pos: Pos[K, F], targetView: View): Unit = pos match {

    case noun: Noun => setDeclensionsTo(noun, targetView)
    case verb: Verb => setDeclensionsTo(verb, targetView)
    case adj: Adjective => setDeclensionsTo(adj, targetView)
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
