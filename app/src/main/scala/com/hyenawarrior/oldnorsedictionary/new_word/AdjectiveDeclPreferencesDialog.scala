package com.hyenawarrior.oldnorsedictionary.new_word

import android.app.{Activity, AlertDialog}
import android.content.{Context, DialogInterface}
import android.view.{LayoutInflater, View}
import android.widget.{Button, RadioButton}
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.core.{AdjectiveFormType, fromTuple}
import com.hyenawarrior.OldNorseGrammar.grammar.adjectival.enums.AdjectiveType
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Case._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.GNumber._
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Gender._
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers.AddNewAdjectiveHelper
import com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers.AddNewAdjectiveHelper.{GRAY, RED}

/**
  * Created by HyenaWarrior on 2018.10.17..
  */
class AdjectiveDeclPreferencesDialog(activity: Activity) {

  private var callback: AdjectiveFormType => Unit = _
  private var state: AdjectiveFormType = AddNewAdjectiveHelper.DEFAULT_ADJECTIVE_TYPE

  def show(onDeclensionSelected: AdjectiveFormType => Unit): Unit = {

    callback = onDeclensionSelected

    dialog.show()
  }

  private object DialogOnClickListener extends DialogInterface.OnClickListener {

    override def onClick(dialogInterface: DialogInterface, option: Int): Unit = option match {

      case DialogInterface.BUTTON_POSITIVE => callback(state)
      //case DialogInterface.BUTTON_NEGATIVE => ()
      case _ => ()
    }
  }

  private object CaseRadioButtonListener extends View.OnClickListener {

    override def onClick(view: View): Unit = {

      state = view.getId match {

        case R.id.rbAdjNom => (state.adjType, state.number, state.gender, NOMINATIVE)
        case R.id.rbAdjAcc => (state.adjType, state.number, state.gender, ACCUSATIVE)
        case R.id.rbAdjDat => (state.adjType, state.number, state.gender, DATIVE)
        case R.id.rbAdjGen => (state.adjType, state.number, state.gender, GENITIVE)
        case _ => state
      }

      disableOtherRadioButtonsThan(view.getId)
    }

    private def disableOtherRadioButtonsThan(id: Int) = {

      val otherRbs = Seq(R.id.rbAdjNom, R.id.rbAdjAcc, R.id.rbAdjDat, R.id.rbAdjGen)
          .filterNot(_ == id)

      for(id <- otherRbs) {

        selfView.findViewById[RadioButton](id).setChecked(false)
      }
    }
  }

  private object GenderRadioButtonListener extends View.OnClickListener {

    override def onClick(view: View): Unit = state = view.getId match {

      case R.id.rbAdjMasculine => (state.adjType, state.number, MASCULINE, state.caze)
      case R.id.rbAdjFeminine  => (state.adjType, state.number, FEMININE, state.caze)
      case R.id.rbAdjNeuter    => (state.adjType, state.number, NEUTER, state.caze)
    }
  }

  private object NumberRadioButtonListener extends View.OnClickListener {

    override def onClick(view: View): Unit = state = view.getId match {

      case R.id.rbAdjSingular => (state.adjType, SINGULAR, state.gender, state.caze)
      case R.id.rbAdjPlural   => (state.adjType, PLURAL,   state.gender, state.caze)
    }
  }

  private object KindButtonListener extends View.OnClickListener {

    override def onClick(view: View): Unit = {

      state = view.getId match {

        case R.id.btAdjPosDef   => (AdjectiveType.POSITIVE_DEFINITE,      state.number, state.gender, state.caze)
        case R.id.btAdjPosIndef => (AdjectiveType.POSITIVE_INDEFINITE,    state.number, state.gender, state.caze)
        case R.id.btAdjCmp      => (AdjectiveType.COMPARATIVE,            state.number, state.gender, state.caze)
        case R.id.btAdjSupDef   => (AdjectiveType.SUPERLATIVE_DEFINITE,   state.number, state.gender, state.caze)
        case R.id.btAdjSupIndef => (AdjectiveType.SUPERLATIVE_INDEFINITE, state.number, state.gender, state.caze)
      }

      disableOtherRadioButtonsThan(view.getId)
    }

    private def disableOtherRadioButtonsThan(selectedId: Int) = {

      selfView.findViewById[Button](selectedId).setBackgroundColor(RED)

      val otherRbs = Seq(R.id.btAdjPosDef, R.id.btAdjPosIndef, R.id.btAdjCmp, R.id.btAdjSupDef, R.id.btAdjSupIndef)
        .filterNot(_ == selectedId)

      for(id <- otherRbs) {

        selfView.findViewById[Button](id).setBackgroundColor(GRAY)
      }
    }
  }

  private val selfView = {

    val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

    inflater.inflate(R.layout.new_adj_overriding_def_row_preferences, null)
  }

  for(id <- Seq(R.id.rbAdjNom, R.id.rbAdjAcc, R.id.rbAdjDat, R.id.rbAdjGen)) {

    selfView.findViewById[View](id).setOnClickListener(CaseRadioButtonListener)
  }

  for(id <- Seq(R.id.rbAdjMasculine, R.id.rbAdjFeminine, R.id.rbAdjNeuter)) {

    selfView.findViewById[View](id).setOnClickListener(GenderRadioButtonListener)
  }

  for(id <- Seq(R.id.rbAdjSingular, R.id.rbAdjPlural)) {

    selfView.findViewById[View](id).setOnClickListener(NumberRadioButtonListener)
  }

  for(id <- Seq(R.id.btAdjPosDef, R.id.btAdjPosIndef, R.id.btAdjCmp, R.id.btAdjSupDef, R.id.btAdjSupIndef)) {

    selfView.findViewById[Button](id).setOnClickListener(KindButtonListener)
  }

  private val dialog = new AlertDialog.Builder(activity)
    //.setTitle("Set Verb Declension")
    .setView(selfView)
    .setPositiveButton("Accept", DialogOnClickListener)
    .setNegativeButton("Cancel", DialogOnClickListener)
    .create()
}
