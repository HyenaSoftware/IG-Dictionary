package com.example.hyenawarrior.myapplication.new_word.pages

import android.content.Context
import android.os.Bundle
import android.support.v4.app.Fragment
import android.view.{LayoutInflater, View, ViewGroup}
import android.widget.AdapterView.OnItemSelectedListener
import android.widget._
import com.example.hyenawarrior.dictionary.modelview.{EditTextTypeListener, SpinnerListener}
import com.example.hyenawarrior.myapplication.R
import com.example.hyenawarrior.myapplication.new_word.new_pos_helpers.{AddNewNounHelper, AddNewNullHelper, AddNewPosHelper, AddNewVerbHelper}

object AddNewWordActivity extends Fragment
{
	outer =>

	var currentPosHelper: AddNewPosHelper = AddNewNullHelper

	//
	class LazyPostInit(val rootView: View) {

		trait Listener extends OnItemSelectedListener	{

			override def onNothingSelected(adapterView: AdapterView[_]) = ()
		}

		object StemClassListener extends Listener {

			override def onItemSelected(adapterView: AdapterView[_], view: View, i: Int, l: Long): Unit = {

				currentPosHelper.onStemClassSelected(i)
			}
		}

		object DeclensionListener extends Listener {

			override def onItemSelected(adapterView: AdapterView[_], view: View, i: Int, l: Long): Unit = {

				currentPosHelper.onDeclensionSelected(i)
			}
		}

		val tlOverrides = rootView.findViewById(R.id.tlOverrides).asInstanceOf[TableLayout]

		val SP_SELECT_STEM_CLASS = rootView.findViewById(R.id.spSelectStemClass).asInstanceOf[Spinner]
		SP_SELECT_STEM_CLASS.setOnItemSelectedListener(StemClassListener)

		val spPosDeclension = rootView.findViewById(R.id.spNounDecl).asInstanceOf[Spinner]
		spPosDeclension.setOnItemSelectedListener(DeclensionListener)

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

		//
		val etPriText = rootView.findViewById(R.id.etNewWord_PriText).asInstanceOf[EditText]
		etPriText.addTextChangedListener(new EditTextTypeListener(onPrimaryTextChange))

		rootView
  }

	//
	private def makeFormOverrideTextListener(view: View) = new EditTextTypeListener(onTextFormOverride(view))

	// forwarders
	def onRemoveOverride(view: View) = view.getTag match
	{
		case tableRow: TableRow =>
			postInitContext.tlOverrides.removeView(tableRow)
			currentPosHelper.onRemoveOverride(tableRow)

		case _ => ()
	}

	private def onTextFormOverride(overridingView: View)(str: String): Unit = currentPosHelper.onTextFormOverride(overridingView)(str)

	private def onPrimaryTextChange(str: String): Unit = currentPosHelper.onPrimaryTextChange(str)

	// router
	private def onPosTypeSelected(newPosType: AddNewPosHelper) {

		currentPosHelper.deactivate()

		currentPosHelper = newPosType

		currentPosHelper.activate()
	}

	//
	def addNewOverride(view: View) = if(postInitContext.tlOverrides.getChildCount < 8)
	{
		val inflater = getActivity.getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
		val rowView = inflater.inflate(R.layout.new_word_overriding_def_row, null)

		// add hint, it's necessary to be able to remove the overrides
		val btnView = rowView.findViewById(R.id.ibRemove)
		btnView.setTag(rowView)

		// add type listeners
		val etView = rowView.findViewById(R.id.etNewWord_Text).asInstanceOf[EditText]
		etView.addTextChangedListener(makeFormOverrideTextListener(rowView))

		//
		val spNounDecl = rowView.findViewById(R.id.spNounDecl).asInstanceOf[Spinner]
		spNounDecl.setOnItemSelectedListener(postInitContext.DeclensionListener)

		postInitContext.tlOverrides.addView(rowView)
	}

	//
	def getWordFormsBy(view: View): WordData = currentPosHelper.getWordFormsBy(view)
}
