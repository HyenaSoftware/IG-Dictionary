package com.example.hyenawarrior.myapplication.new_word.pages

import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import com.example.hyenawarrior.myapplication.R

/**
	* Created by HyenaWarrior on 2017.04.27..
	*/
object SetMeaningFragment extends Fragment
{
	override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle): View = {

		val rootView = inflater.inflate(R.layout.add_meaning_for_new_word, container, false)

		rootView
	}
}
