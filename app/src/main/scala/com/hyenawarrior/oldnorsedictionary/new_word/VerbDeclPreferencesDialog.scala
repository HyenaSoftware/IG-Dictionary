package com.hyenawarrior.oldnorsedictionary.new_word

import android.app.{Activity, AlertDialog}
import android.content.{Context, DialogInterface}
import android.view.{LayoutInflater, View}
import android.widget.AdapterView.OnItemSelectedListener
import android.widget.{AdapterView, ArrayAdapter, RadioButton, Spinner}
import com.hyenawarrior.OldNorseGrammar.grammar.Pronoun
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.{VerbModeEnum, VerbTenseEnum, VerbType}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.model.database.marshallers.VerbForm
import com.hyenawarrior.oldnorsedictionary.new_word.VerbDeclPreferencesDialog._

/**
	* Created by HyenaWarrior on 2017.06.17..
	*/
object VerbDeclPreferencesDialog
{
	val spinnerItemsIndPresentItem			= List(
		VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG.vtype,
		VerbForm.VERB_INDICATIVE_PRESENT_2ND_SG.vtype,
		VerbForm.VERB_INDICATIVE_PRESENT_3RD_SG.vtype,

		VerbForm.VERB_INDICATIVE_PRESENT_1ST_PL.vtype,
		VerbForm.VERB_INDICATIVE_PRESENT_2ND_PL.vtype,
		VerbForm.VERB_INDICATIVE_PRESENT_3RD_PL.vtype)

	val spinnerItemsIndPreteriteItem			= List(
		VerbForm.VERB_INDICATIVE_PAST_1ST_SG.vtype,
		VerbForm.VERB_INDICATIVE_PAST_2ND_SG.vtype,
		VerbForm.VERB_INDICATIVE_PAST_3RD_SG.vtype,

		VerbForm.VERB_INDICATIVE_PAST_1ST_PL.vtype,
		VerbForm.VERB_INDICATIVE_PAST_2ND_PL.vtype,
		VerbForm.VERB_INDICATIVE_PAST_3RD_PL.vtype)

	val spinnerItemsSubjPresentItem			= List(
		VerbForm.VERB_SUBJUNCTIVE_PRESENT_1ST_SG.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PRESENT_2ND_SG.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PRESENT_3RD_SG.vtype,

		VerbForm.VERB_SUBJUNCTIVE_PRESENT_1ST_PL.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PRESENT_2ND_PL.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PRESENT_3RD_PL.vtype)

	val spinnerItemsSubjPreteriteItem			= List(
		VerbForm.VERB_SUBJUNCTIVE_PAST_1ST_SG.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PAST_2ND_SG.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PAST_3RD_SG.vtype,

		VerbForm.VERB_SUBJUNCTIVE_PAST_1ST_PL.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PAST_2ND_PL.vtype,
		VerbForm.VERB_SUBJUNCTIVE_PAST_3RD_PL.vtype)

	val spinnerItemsNonFinitive		=  List(
			VerbForm.VERB_INFINITIVE.vtype,
			VerbForm.VERB_PRESENT_PARTICIPLE.vtype,
			VerbForm.VERB_PAST_PARTICIPLE.vtype)

  val spinnerItemsImperative = List(
			VerbForm.VERB_IMPERATIVE_SG_2ND.vtype,
			VerbForm.VERB_IMPERATIVE_PL_1ST.vtype,
			VerbForm.VERB_IMPERATIVE_PL_2ND.vtype)
}

class VerbDeclPreferencesDialog(activity: Activity)
{
	type State = (VerbModeEnum, Option[VerbTenseEnum], Option[Pronoun])

	var state = VerbForm.VERB_INDICATIVE_PRESENT_1ST_SG.vtype
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
		val verbDeclSpinner: Spinner = selfView.findViewById(R.id.spVerbDeclensions).asInstanceOf[Spinner]
		verbDeclSpinner.setOnItemSelectedListener(SpinnerListener)

		override def onClick(view: View): Unit = view.getId match {

			case id @ (R.id.rbInd | R.id.rbSubj) =>
				activeVerbMode = id
				setSpinnerAndTenseRadioButtons(R.array.verb_ind_subj_decls, true)
        disableOtherRadioButtonsThan(id)
				setState()

			case R.id.rbNonFinitive =>
				activeVerbMode = R.id.rbNonFinitive
				setSpinnerAndTenseRadioButtons(R.array.verb_nonfinitive_decls, false)
        disableOtherRadioButtonsThan(R.id.rbNonFinitive)
				setState()

      case R.id.rbImperative =>
        activeVerbMode = R.id.rbImperative

        rbPresent.setChecked(true)
        rbPreterite.setChecked(false)

        setSpinnerAndTenseRadioButtons(R.array.verb_imperative_decls, false)
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

        selfView.findViewById(id) match {
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

  val rbPresent = selfView.findViewById(R.id.rbPresent).asInstanceOf[RadioButton]
  val rbPreterite = selfView.findViewById(R.id.rbPreterite).asInstanceOf[RadioButton]

	// wire the radio buttons to the event handler
	for(id <- Seq(R.id.rbInd, R.id.rbSubj, R.id.rbImperative, R.id.rbNonFinitive, R.id.rbPresent, R.id.rbPreterite))
	{
		selfView.findViewById(id).setOnClickListener(RadioButtonListener)
	}

	private val dialog = new AlertDialog.Builder(activity)
		//.setTitle("Set Verb Declension")
		.setView(selfView)
		.setPositiveButton("Accept", DialogOnClickListener)
		.setNegativeButton("Cancel", DialogOnClickListener)
		.create()
}
