package com.hyenawarrior.oldnorsedictionary.new_word.pages

import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget._
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.modelview.{ItemListener, SpinnerListener}
import com.hyenawarrior.oldnorsedictionary.new_word.new_pos_helpers._

object AddNewWordActivity extends Fragment
{
	outer =>

	var currentPosHelper: AddNewPosHelper = AddNewNullHelper

	//
	class LazyPostInit(val rootView: View) {

		object StemClassListener extends ItemListener(i => currentPosHelper.onStemClassSelected(i))

		val tlOverrides = rootView.findViewById[TableLayout](R.id.tlOverrides)

		val SP_SELECT_STEM_CLASS = rootView.findViewById[Spinner](R.id.spSelectStemClass)
		SP_SELECT_STEM_CLASS.setOnItemSelectedListener(StemClassListener)

		val addNewNounHelper = new AddNewNounHelper(rootView, getActivity, SP_SELECT_STEM_CLASS)
		val addNewVerbHelper = new AddNewVerbHelper(rootView, getActivity, SP_SELECT_STEM_CLASS)
		val addNewAdjectiveHelper = new AddNewAdjectiveHelper(rootView, getActivity, SP_SELECT_STEM_CLASS)

		val generatedItems = rootView.findViewById[LinearLayout](R.id.llGeneratedItems)

		val POS_TYPES = Vector(addNewNounHelper, addNewVerbHelper, addNewAdjectiveHelper)
	}

	var postInitContext: LazyPostInit = null

	override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle): View = {

		val rootView = inflater.inflate(R.layout.activity_add_new_word, container, false)

		postInitContext = new LazyPostInit(rootView)

		//
		val posTypeSpinnerListener = new SpinnerListener(postInitContext.POS_TYPES, onPosTypeSelected)
		val spSelectPoS = rootView.findViewById[Spinner](R.id.spSelectPoS)
		spSelectPoS.setOnItemSelectedListener(posTypeSpinnerListener)

		rootView
  }

	// forwarders
	def onRemoveOverride(view: View) = view.getTag match
	{
		case tableRow: View =>
			postInitContext.tlOverrides.removeView(tableRow)
			currentPosHelper.onRemoveOverride(tableRow)

		case _ => ()
	}

	// router
	private def onPosTypeSelected(newPosType: AddNewPosHelper) {

		postInitContext.tlOverrides.removeAllViews()
		postInitContext.generatedItems.removeAllViews()

		currentPosHelper.deactivate()

		currentPosHelper = newPosType

		currentPosHelper.activate()

		postInitContext.tlOverrides.addView(currentPosHelper.primaryFromSetter())
	}

	//
	def addNewOverride(view: View) = if(postInitContext.tlOverrides.getChildCount < 8)
	{
		val newForm = currentPosHelper.createOverrideFormSetter()

		postInitContext.tlOverrides.addView(newForm)
	}

	//
	def getWordFormsBy(view: View): WordData = currentPosHelper.getWordFormsBy(view)
}
