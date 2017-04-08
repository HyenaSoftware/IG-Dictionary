package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.content.Context
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{LayoutInflater, View}
import android.widget.{EditText, TableLayout, TableRow}
import com.example.hyenawarrior.myapplication.{MainActivity, R}
import com.hyenawarrior.OldNorseGrammar.grammar.{Case, Number}

class AddNewWordActivity extends AppCompatActivity
{
	outer =>

	lazy val tlOverrides = findViewById(R.id.tlOverrides).asInstanceOf[TableLayout]

	val typeListener = new TypeListener(this)

	protected override def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState);

    setContentView(R.layout.activity_add_new_word);

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE)

		val etPriText = findViewById(R.id.etNewWord_PriText).asInstanceOf[EditText]
		etPriText.addTextChangedListener(typeListener)
  }

	def onRemoveOverride(view: View) = view.getTag match
	{
		case tableRow: TableRow => tlOverrides.removeView(tableRow)
		case _ => ()
	}

	def addNewOverride(view: View) =
	{
		val inflater = getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
		val rowView = inflater.inflate(R.layout.new_word_overriding_def_row, null)

		// add hint
		val btnView = rowView.findViewById(R.id.ibRemove)
		btnView.setTag(rowView)

		// add type listeners
		val etView = rowView.findViewById(R.id.etNewWord_Text).asInstanceOf[EditText]
		etView.addTextChangedListener(typeListener)

		tlOverrides.addView(rowView)
	}
}
