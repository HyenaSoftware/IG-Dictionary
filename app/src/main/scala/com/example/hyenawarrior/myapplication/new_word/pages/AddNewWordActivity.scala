package com.example.hyenawarrior.myapplication.new_word.pages

import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget._
import com.example.hyenawarrior.dictionary.modelview.{EditTextTypeListener, ItemListener, SpinnerListener}
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.myapplication.new_word.new_pos_helpers.{AddNewNounHelper, AddNewNullHelper, AddNewPosHelper, AddNewVerbHelper}

object AddNewWordActivity extends Fragment
{
	outer =>

	var currentPosHelper: AddNewPosHelper = AddNewNullHelper

	//
	class LazyPostInit(val rootView: View) {

		object StemClassListener extends ItemListener(i => currentPosHelper.onStemClassSelected(i))

		val tlOverrides = rootView.findViewById(R.id.tlOverrides).asInstanceOf[TableLayout]

		val SP_SELECT_STEM_CLASS = rootView.findViewById(R.id.spSelectStemClass).asInstanceOf[Spinner]
		SP_SELECT_STEM_CLASS.setOnItemSelectedListener(StemClassListener)

		val addNewNounHelper = new AddNewNounHelper(rootView, getActivity, SP_SELECT_STEM_CLASS)
		val addNewVerbHelper = new AddNewVerbHelper(rootView, getActivity, SP_SELECT_STEM_CLASS)

		//val NounDeclensionAdapter = new NounDeclensionAdapter(outer.getActivity)

		val POS_TYPES = Vector(addNewNounHelper, addNewVerbHelper, AddNewNullHelper)
		//val LL_DECL_LIST = rootView.findViewById(R.id.llDeclensionList).asInstanceOf[LinearLayout]
	}

	var postInitContext: LazyPostInit = null

	override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle): View = {

		val rootView = inflater.inflate(R.layout.activity_add_new_word, container, false)

		postInitContext = new LazyPostInit(rootView)

		//
		val posTypeSpinnerListener = new SpinnerListener(postInitContext.POS_TYPES, onPosTypeSelected)
		val spSelectPoS = rootView.findViewById(R.id.spSelectPoS).asInstanceOf[Spinner]
		spSelectPoS.setOnItemSelectedListener(posTypeSpinnerListener)

		rootView
  }

	//
	private def makeFormOverrideTextListener(view: View) = new EditTextTypeListener(onTextFormOverride(view))

	// forwarders
	def onRemoveOverride(view: View) = view.getTag match
	{
		case tableRow: View =>
			postInitContext.tlOverrides.removeView(tableRow)
			currentPosHelper.onRemoveOverride(tableRow)

		case _ => ()
	}

	private def onTextFormOverride(overridingView: View)(str: String): Unit = currentPosHelper.onTextFormOverride(overridingView)(str)

	private def onPrimaryTextChange(str: String): Unit = currentPosHelper.onPrimaryTextChange(str)

	// router
	private def onPosTypeSelected(newPosType: AddNewPosHelper) {

		postInitContext.tlOverrides.removeAllViews()

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
