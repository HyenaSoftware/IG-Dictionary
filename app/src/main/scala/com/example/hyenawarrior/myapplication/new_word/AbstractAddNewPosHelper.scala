package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.widget.{ArrayAdapter, Spinner}
import com.example.hyenawarrior.myapplication.R

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
abstract class AbstractAddNewPosHelper[T](activity: Activity, spinnerItemRsrc: Int) extends AddNewPosHelper
{
	private val stemClassSpinnerListener = new SpinnerListener(loadStemClassEnums, onStemClassSelected)

	def activate(): Unit = {

		val spSelectStemClass = activity.findViewById(R.id.spSelectStemClass).asInstanceOf[Spinner]

		//
		val adapter = ArrayAdapter.createFromResource(activity,	spinnerItemRsrc, android.R.layout.simple_spinner_item)
		adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
		spSelectStemClass.setAdapter(adapter)

		//
		spSelectStemClass.setOnItemSelectedListener(stemClassSpinnerListener)
	}

	def deactivate(): Unit = {

		val spSelectStemClass = activity.findViewById(R.id.spSelectStemClass).asInstanceOf[Spinner]

		spSelectStemClass.setOnItemSelectedListener(null)
	}

	protected def onStemClassSelected(newStemClassList: List[T])

	protected def loadStemClassEnums: Vector[List[T]]
}

