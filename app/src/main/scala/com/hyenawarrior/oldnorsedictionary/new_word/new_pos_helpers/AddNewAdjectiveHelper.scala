package com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View}
import android.widget._
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.Adjective
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.AdjectiveFormType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender.{FEMININE, MASCULINE, NEUTER}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.{Case, GNumber}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview.EditTextTypeListener
import com.hyenawarrior.oldnorsedictionary.modelview.add_new_word_panel.AdjectiveDeclensionAdapter
import com.hyenawarrior.oldnorsedictionary.new_word.AdjectiveDeclPreferencesDialog
import com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers.AddNewAdjectiveHelper.{DEFAULT_ADJECTIVE_TYPE, GRAY, RED}
import com.hyenawarrior.oldnorsedictionary.new_word.pages.AddNewWordActivity._

/**
  * Created by HyenaWarrior on 2018.10.15..
  */
object AddNewAdjectiveHelper {

  val NOUN_DECLENSIONS = Vector(
    (SINGULAR, NOMINATIVE),
    (SINGULAR, ACCUSATIVE),
    (SINGULAR, DATIVE),
    (SINGULAR, GENITIVE),

    (PLURAL, NOMINATIVE),
    (PLURAL, ACCUSATIVE),
    (PLURAL, DATIVE),
    (PLURAL, GENITIVE)
  )

  type Declension = (GNumber, Case)

  val DEFAULT_ADJECTIVE_TYPE = AdjectiveFormType(POSITIVE_INDEFINITE, SINGULAR, MASCULINE, NOMINATIVE)

  val RED = 0xffcc0000
  val GRAY = 0xffaaaaaa
}

class AddNewAdjectiveHelper(rootView: View, activity: Activity, stemClassSpinner: Spinner)
  extends AbstractAddNewPosHelper(activity, stemClassSpinner, R.array.adj_types) {

  val CONTROL_PANEL = rootView.findViewById[LinearLayout](R.id.llAdjControlPanel)
  val ADJECTIVE_FORMS = rootView.findViewById[LinearLayout](R.id.llGeneratedItems)

  val AdjectiveDeclensionAdapter = new AdjectiveDeclensionAdapter(activity, ADJECTIVE_FORMS)

  val adjDeclPreferencesDialog = new AdjectiveDeclPreferencesDialog(activity)

  var inputData: Map[View, (AdjectiveFormType, String)] = Map()
  var kindToGenerate: List[AdjectiveType] = List()

  private val KINDS_IN_SPINNER = List(
    List(POSITIVE_INDEFINITE, POSITIVE_DEFINITE),
    List(COMPARATIVE, SUPERLATIVE_INDEFINITE, SUPERLATIVE_DEFINITE),
    List(POSITIVE_INDEFINITE, POSITIVE_DEFINITE, COMPARATIVE, SUPERLATIVE_INDEFINITE, SUPERLATIVE_DEFINITE)
  )

  case class ShowAdjDeclDialogListener(inputRowAsKey: View) extends View.OnClickListener {

    onDeclensionSelected(DEFAULT_ADJECTIVE_TYPE)

    override def onClick(view: View): Unit = adjDeclPreferencesDialog.show(onDeclensionSelected)

    private def updateKindLED(adjFormType: AdjectiveFormType) = {

      val (clrCmp, clrSup) = adjFormType.adjType match {

        case COMPARATIVE => (RED, GRAY)
        case SUPERLATIVE_DEFINITE | SUPERLATIVE_INDEFINITE => (GRAY, RED)
        case _ => (GRAY, GRAY)
      }

      inputRowAsKey.findViewById[TextView](R.id.tvAdjComparative).setTextColor(clrCmp)
      inputRowAsKey.findViewById[TextView](R.id.tvAdjSuperlative).setTextColor(clrSup)
    }

    private def updateDefinitenessLED(adjFormType: AdjectiveFormType) = {

      val (clrDef, clrIndef) = adjFormType.adjType match {

        case POSITIVE_DEFINITE | COMPARATIVE | SUPERLATIVE_DEFINITE => (RED, GRAY)
        case POSITIVE_INDEFINITE | SUPERLATIVE_INDEFINITE => (GRAY, RED)
      }

      inputRowAsKey.findViewById[TextView](R.id.tvAdjDefinite).setTextColor(clrDef)
      inputRowAsKey.findViewById[TextView](R.id.tvAdjIndefinite).setTextColor(clrIndef)
    }

    private def updateNumberLED(adjFormType: AdjectiveFormType) = {

      val strNumber = adjFormType.number match {

        case SINGULAR => "SG"
        case PLURAL => "PL"
      }

      inputRowAsKey.findViewById[TextView](R.id.tvAdjNumber).setText(strNumber)
    }

    private def updateGenderLED(adjFormType: AdjectiveFormType) = {

      val strGender = adjFormType.gender match {

        case MASCULINE => "MASC"
        case FEMININE => "FEM"
        case NEUTER => "NEUT"
      }

      inputRowAsKey.findViewById[TextView](R.id.tvAdjGender).setText(strGender)
    }

    private def updateCaseLED(adjFormType: AdjectiveFormType) = {

      val strCase = adjFormType.caze match {

        case NOMINATIVE => "NOM"
        case ACCUSATIVE => "ACC"
        case DATIVE => "DAT"
        case GENITIVE => "GEN"
      }

      inputRowAsKey.findViewById[TextView](R.id.tvAdjCase).setText(strCase)
    }

    private def onDeclensionSelected(adjFormType: AdjectiveFormType): Unit = {

      val entry = (adjFormType, inputData.get(inputRowAsKey).map(_._2).getOrElse(""))

      inputData = (inputData - inputRowAsKey) + (inputRowAsKey -> entry)

      updateKindLED(adjFormType)
      updateDefinitenessLED(adjFormType)
      updateNumberLED(adjFormType)
      updateGenderLED(adjFormType)
      updateCaseLED(adjFormType)

      tryCompleteForms()
    }
  }

  override def activate(): Unit = {

    super.activate()

    CONTROL_PANEL.setVisibility(View.VISIBLE)
  }

  override def deactivate(): Unit = {

    super.deactivate()

    CONTROL_PANEL.setVisibility(View.GONE)
  }

  override def onRemoveOverride(tableRow: View): Unit = {

    inputData -= tableRow

    tryCompleteForms()
  }

  override def createOverrideFormSetter(isPrimary: Boolean): View = {

    val inflater = getActivity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
    val rowView = inflater.inflate(R.layout.new_adj_overriding_def_row, null)

    // add hint, it's necessary to be able to remove the overrides
    val btnRemoveView = rowView.findViewById[View](R.id.ibRemove)
    btnRemoveView.setTag(rowView)
    btnRemoveView.setVisibility(if(isPrimary) View.GONE else View.VISIBLE)

    //
    val btnPref = rowView.findViewById[View](R.id.ibPreferences)
    btnPref.setOnClickListener(ShowAdjDeclDialogListener(rowView))

    // add text listeners
    val etView = rowView.findViewById[EditText](R.id.etNewWord_Text)
    val etListener = new EditTextTypeListener(onTextFormOverride(rowView))

    etView.addTextChangedListener(etListener)

    rowView
  }

  override def onStemClassSelected(index: Int): Unit = {

    kindToGenerate = KINDS_IN_SPINNER(index)

    tryCompleteForms()
  }

  private def tryCompleteForms(): Unit = try {

    val inputForms = inputData.values.toMap

    val adj = Adjective.from(inputForms, kindToGenerate.toSet)

    AdjectiveDeclensionAdapter resetItems kindToGenerate.map(_ -> adj)

  } catch {

    case e: RuntimeException =>
    val msg = Option(e.getMessage).getOrElse("")

    android.util.Log.w(this.getClass.getSimpleName, msg)

    ()
  }

  override def onTextFormOverride(overridingView: View)(strForm: String): Unit = {

    val entry = (inputData.get(overridingView).map(_._1).getOrElse(DEFAULT_ADJECTIVE_TYPE), strForm)

    inputData = (inputData - overridingView) + (overridingView -> entry)

    tryCompleteForms()
  }
}