package com.example.hyenawarrior.myapplication.new_word

import android.content.Context
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.view.{LayoutInflater, View}
import android.widget.AdapterView.OnItemSelectedListener
import android.widget._
import com.example.hyenawarrior.dictionary.modelview.add_new_word_panel.NounDeclensionAdapter
import com.example.hyenawarrior.myapplication.{MainActivity, R}

class AddNewWordActivity extends AppCompatActivity
{
	outer =>

	var currentPosHelper: AddNewPosHelper = AddNewNullHelper

	//
	class LazyPostInit {

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

		val tlOverrides = findViewById(R.id.tlOverrides).asInstanceOf[TableLayout]

		val SP_SELECT_STEM_CLASS = findViewById(R.id.spSelectStemClass).asInstanceOf[Spinner]
		SP_SELECT_STEM_CLASS.setOnItemSelectedListener(StemClassListener)

		val spPosDeclension = findViewById(R.id.spNounDecl).asInstanceOf[Spinner]
		spPosDeclension.setOnItemSelectedListener(DeclensionListener)

		val addNewNounHelper = new AddNewNounHelper(outer, SP_SELECT_STEM_CLASS)
		val addNewVerbHelper = new AddNewVerbHelper(outer, SP_SELECT_STEM_CLASS)

		val NounDeclensionAdapter = new NounDeclensionAdapter(outer)

		val POS_TYPES = Vector(addNewNounHelper, addNewVerbHelper, AddNewNullHelper)
		val LL_DECL_LIST = findViewById(R.id.llDeclensionList).asInstanceOf[LinearLayout]
	}

	lazy val postInitContext = new LazyPostInit

	protected override def onCreate(savedInstanceState: Bundle)
	{
		super.onCreate(savedInstanceState)

    setContentView(R.layout.activity_add_new_word)

		//https://developer.android.com/training/basics/firstapp/starting-activity.html
		// Get the Intent that started this activity and extract the string
		val intent = getIntent
		val message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE)

		//
		val posTypeSpinnerListener = new SpinnerListener(postInitContext.POS_TYPES, onPosTypeSelected)
		val spSelectPoS = findViewById(R.id.spSelectPoS).asInstanceOf[Spinner]
		spSelectPoS.setOnItemSelectedListener(posTypeSpinnerListener)

		//
		val etPriText = findViewById(R.id.etNewWord_PriText).asInstanceOf[EditText]
		etPriText.addTextChangedListener(new EditTextTypeListener(this, onPrimaryTextChange))
  }



	//
	def makeFormOverrideTextListener(view: View) = new EditTextTypeListener(this, onTextFormOverride(view))

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
		val inflater = getSystemService(Context.LAYOUT_INFLATER_SERVICE).asInstanceOf[LayoutInflater]
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
}
