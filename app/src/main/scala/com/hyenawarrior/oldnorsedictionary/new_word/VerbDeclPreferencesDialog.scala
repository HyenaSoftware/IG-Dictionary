package com.hyenawarrior.oldnorsedictionary.new_word

import android.app.{Activity, AlertDialog}
import android.content.{Context, DialogInterface}
import android.view.{LayoutInflater, View}
import android.widget.AdapterView.OnItemSelectedListener
import android.widget.{AdapterView, ArrayAdapter, RadioButton, Spinner}
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.enums.Pronoun._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbModeEnum._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbTenseEnum.{PAST, PRESENT}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbVoice._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.VerbType
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.{VerbModeEnum, VerbTenseEnum}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.new_word.VerbDeclPreferencesDialog._
import com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers.AddNewVerbHelper

/**
	* Created by HyenaWarrior on 2017.06.17..
	*/
object VerbDeclPreferencesDialog
{
  val spinnerItemsIndPresentItem    = List(SG_1, SG_2, SG_3, PL_1, PL_2, PL_3)
    .map { pronoun => (INDICATIVE, ACTIVE, Some(PRESENT), Some(pronoun)) }

  val spinnerItemsIndPreteriteItem  = List(SG_1, SG_2, SG_3, PL_1, PL_2, PL_3)
    .map { pronoun => (INDICATIVE, ACTIVE, Some(PAST), Some(pronoun)) }

  val spinnerItemsSubjPresentItem	  = List(SG_1, SG_2, SG_3, PL_1, PL_2, PL_3)
    .map { pronoun => (SUBJUNCTIVE, ACTIVE, Some(PRESENT), Some(pronoun)) }

	val spinnerItemsSubjPreteriteItem = List(SG_1, SG_2, SG_3, PL_1, PL_2, PL_3)
    .map { pronoun => (SUBJUNCTIVE, ACTIVE, Some(PAST), Some(pronoun)) }

  val spinnerItemsNonFinitive	= List[VerbType](
    (INFINITIVE, ACTIVE, None,          None),
    (PARTICIPLE, ACTIVE, Some(PRESENT), None),
    (PARTICIPLE, ACTIVE, Some(PAST),    None))

  val spinnerItemsImperative = List(SG_2, PL_1, PL_2)
    .map { e => (IMPERATIVE, ACTIVE, Some(PRESENT), Some(e)) }
}

class VerbDeclPreferencesDialog(activity: Activity)
{
	private type State = (VerbModeEnum, Option[VerbTenseEnum], Option[Pronoun])

	var state = AddNewVerbHelper.DEFAULT_VERB_TYPE

	var activeVerbMode = R.id.rbInd
	var activeVerbTense = R.id.rbPresent
	var spinnerIndex = 0

	var isReflexive = false
	var callback: Option[Option[VerbType] => Unit] = None

	object DialogOnClickListener extends DialogInterface.OnClickListener {

		override def onClick(dialogInterface: DialogInterface, i: Int): Unit = if(callback.isDefined) i match {

			case DialogInterface.BUTTON_POSITIVE => callback.get(Some(state))
			case DialogInterface.BUTTON_NEGATIVE => callback.get(None)
		}
	}

	object SpinnerListener extends OnItemSelectedListener {

		override def onNothingSelected(adapterView: AdapterView[_]): Unit = ???

		override def onItemSelected(adapterView: AdapterView[_], view: View, idx: Int, l: Long): Unit = {

			spinnerIndex = idx
			setState()
		}
	}

	private def setState(): Unit = {

		(activeVerbMode, activeVerbTense) match {

			case (R.id.rbInd, R.id.rbPresent) => state = spinnerItemsIndPresentItem(spinnerIndex)

			case (R.id.rbInd, R.id.rbPreterite) => state = spinnerItemsIndPreteriteItem(spinnerIndex)

			case (R.id.rbSubj, R.id.rbPresent) => state = spinnerItemsSubjPresentItem(spinnerIndex)

			case (R.id.rbSubj, R.id.rbPreterite) => state = spinnerItemsSubjPreteriteItem(spinnerIndex)

			case (R.id.rbNonFinitive, _) => state = spinnerItemsNonFinitive(spinnerIndex)

      case (R.id.rbImperative, _) => state = spinnerItemsImperative(spinnerIndex)
		}
	}

	object RadioButtonListener extends View.OnClickListener
	{
		val verbDeclSpinner: Spinner = selfView.findViewById[Spinner](R.id.spVerbDeclensions)
		verbDeclSpinner.setOnItemSelectedListener(SpinnerListener)

		override def onClick(view: View): Unit = view.getId match {

			case id @ (R.id.rbInd | R.id.rbSubj) =>
				activeVerbMode = id
				setSpinnerAndTenseRadioButtons(R.array.verb_ind_subj_decls, enableTenseButtons = true)
        disableOtherRadioButtonsThan(id)
				setState()

			case R.id.rbNonFinitive =>
				activeVerbMode = R.id.rbNonFinitive
				setSpinnerAndTenseRadioButtons(R.array.verb_nonfinitive_decls, enableTenseButtons = false)
        disableOtherRadioButtonsThan(R.id.rbNonFinitive)
				setState()

      case R.id.rbImperative =>
        activeVerbMode = R.id.rbImperative

        rbPresent.setChecked(true)
        rbPreterite.setChecked(false)

        setSpinnerAndTenseRadioButtons(R.array.verb_imperative_decls, enableTenseButtons = false)
        disableOtherRadioButtonsThan(R.id.rbImperative)
        setState()

			case id @ (R.id.rbPresent | R.id.rbPreterite) =>
				activeVerbTense = id
				setState()
		}

		private def setSpinnerAndTenseRadioButtons(intSpinnerRsrc: Int, enableTenseButtons: Boolean): Unit = {

			val adapter = ArrayAdapter.createFromResource(activity,	intSpinnerRsrc, android.R.layout.simple_spinner_item)
			adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
			verbDeclSpinner.setAdapter(adapter)

			spinnerIndex = 0

      rbPresent.setEnabled(enableTenseButtons)
			rbPreterite.setEnabled(enableTenseButtons)
		}

    private def disableOtherRadioButtonsThan(selectedRb: Int): Unit = {

      val otherRbs = Seq(R.id.rbInd, R.id.rbSubj, R.id.rbImperative, R.id.rbNonFinitive)
        .filterNot(_ == selectedRb)

      for(id <- otherRbs) {

        selfView.findViewById[View](id) match {
          case rb: RadioButton => rb.setChecked(false)
          case _ => ()
        }
      }
    }
	}

	def show(callback: VerbType => Unit) {

		this.callback = Some {

			case Some(v) => callback(v)
			case _ => ()
		}

		dialog.show()
	}

	def showOpt(callback: Option[VerbType] => Unit) {

		this.callback = Some(callback)

		dialog.show()
	}

	private val selfView = {

		val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

		inflater.inflate(R.layout.new_verb_overriding_def_row_preferences, null)
	}

  val rbPresent = selfView.findViewById[RadioButton](R.id.rbPresent)
  val rbPreterite = selfView.findViewById[RadioButton](R.id.rbPreterite)

	// wire the radio buttons to the event handler
	for(id <- Seq(R.id.rbInd, R.id.rbSubj, R.id.rbImperative, R.id.rbNonFinitive, R.id.rbPresent, R.id.rbPreterite))
	{
		selfView.findViewById[View](id).setOnClickListener(RadioButtonListener)
	}

	private val dialog = new AlertDialog.Builder(activity)
		//.setTitle("Set Verb Declension")
		.setView(selfView)
		.setPositiveButton("Accept", DialogOnClickListener)
		.setNegativeButton("Cancel", DialogOnClickListener)
		.create()
}
