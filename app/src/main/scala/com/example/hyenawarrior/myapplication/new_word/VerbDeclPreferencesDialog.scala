package com.example.hyenawarrior.myapplication.new_word

import android.app.{Activity, AlertDialog}
import android.content.{Context, DialogInterface}
import android.view.{LayoutInflater, View}
import android.widget.{ArrayAdapter, Spinner}
import com.example.hyenawarrior.myapplication.R

/**
	* Created by HyenaWarrior on 2017.06.17..
	*/
class VerbDeclPreferencesDialog(activity: Activity)
{
	object DialogOnClickListener extends DialogInterface.OnClickListener
	{
		override def onClick(dialogInterface: DialogInterface, i: Int): Unit = i match
		{
			case DialogInterface.BUTTON_POSITIVE => ()
			case DialogInterface.BUTTON_NEGATIVE => ()
		}
	}

	object RadioButtonListener extends View.OnClickListener
	{
		val verbDeclSpinner: Spinner = selfView.findViewById(R.id.spVerbDeclensions).asInstanceOf[Spinner]

		override def onClick(view: View): Unit = view.getId match
		{
			case R.id.rbInd | R.id.rbSubj => setSpinnerAndTenseRadioButtons(R.array.verb_ind_subj_decls, true)

			case R.id.rbNonFinitive				=> setSpinnerAndTenseRadioButtons(R.array.verb_nonfinitive_decls, false)

			case R.id.rbPresent | R.id.rbPreterite => ()
		}

		private def setSpinnerAndTenseRadioButtons(intSpinnerRsrc: Int, enableTenseButtons: Boolean): Unit =
		{
			val adapter = ArrayAdapter.createFromResource(activity,	intSpinnerRsrc, android.R.layout.simple_spinner_item)
			adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
			verbDeclSpinner.setAdapter(adapter)

			selfView.findViewById(R.id.rbPresent).setEnabled(enableTenseButtons)
			selfView.findViewById(R.id.rbPreterite).setEnabled(enableTenseButtons)
		}
	}


	private val selfView =
	{
		val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

		inflater.inflate(R.layout.new_verb_overriding_def_row_preferences, null)
	}

	// wire the radio buttons to the event handler
	for(id <- Seq(R.id.rbInd, R.id.rbSubj, R.id.rbNonFinitive, R.id.rbPresent, R.id.rbPreterite))
	{
		selfView.findViewById(id).setOnClickListener(RadioButtonListener)
	}

	val dialog = new AlertDialog.Builder(activity)
		//.setTitle("Set Verb Declension")
		.setView(selfView)
		.setPositiveButton("Accept", DialogOnClickListener)
		.setNegativeButton("Cancel", DialogOnClickListener)
		.create()
}
