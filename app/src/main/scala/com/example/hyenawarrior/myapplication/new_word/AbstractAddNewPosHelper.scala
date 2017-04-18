package com.example.hyenawarrior.myapplication.new_word

import android.app.Activity
import android.widget.{ArrayAdapter, Spinner}
import com.example.hyenawarrior.myapplication.R

/**
	* Created by HyenaWarrior on 2017.04.17..
	*/
abstract class AbstractAddNewPosHelper(activity: Activity, spSelectStemClass: Spinner, spinnerItemRsrc: Int) extends AddNewPosHelper
{
	def activate(): Unit = {
		//
		val adapter = ArrayAdapter.createFromResource(activity,	spinnerItemRsrc, android.R.layout.simple_spinner_item)
		adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
		spSelectStemClass.setAdapter(adapter)
	}

	def deactivate(): Unit = {

	}
}

