package com.example.hyenawarrior.myapplication

import android.content.Context
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{LayoutInflater, View}
import android.widget.{TableLayout, TableRow}

class AddNewWordActivity extends AppCompatActivity
{
	lazy val tlOverrides = findViewById(R.id.tlOverrides).asInstanceOf[TableLayout]

	protected override def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState);

    setContentView(R.layout.activity_add_new_word);

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE);
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

		tlOverrides.addView(rowView)
	}
}
