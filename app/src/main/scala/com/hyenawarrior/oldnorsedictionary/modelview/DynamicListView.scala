package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.content.Context
import android.view.{LayoutInflater, View, ViewGroup}

/**
	* Created by HyenaWarrior on 2017.07.22..
	*/
abstract class DynamicListView[T](host: ViewGroup, recordRsrcId: Int, activity: Activity)
{
	private val inflater = activity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]

	protected def applyToView(elem: Option[T], recordView: View)

	def add(): View = add(None)

	def add(elem: T): View = add(Some(elem))

	private def add(elem: Option[T]): View =
	{
		val recordView = inflater.inflate(recordRsrcId, null)

		applyToView(elem, recordView)

		host addView recordView

		recordView
	}

	def remove(view: View)
	{
		host.removeView(view)
	}
}

