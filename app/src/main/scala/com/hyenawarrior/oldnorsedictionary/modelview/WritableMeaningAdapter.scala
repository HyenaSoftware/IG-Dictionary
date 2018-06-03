package com.hyenawarrior.oldnorsedictionary.modelview

import android.app.Activity
import android.graphics.Color
import android.view.{View, ViewGroup}
import android.widget.{EditText, TextView}
import com.hyenawarrior.oldnorsedictionary.R
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef

/**
  * Created by HyenaWarrior on 2018.05.29..
  *
  * + - - - -+ - - - - - +
  * | to fly | meaning1  |
  * |        | meaning2  |
  * + - - - -+ - - - - - +
  */
class WritableMeaningAdapter(activity: Activity, listView: ViewGroup)
  extends CustomAdapter[MeaningDef](activity, listView, R.layout.meanings_writeable) {

  private val EMPTY = MeaningDef("", "", Seq())
  private var eventsAreEnabled = true

  protected override def initView(view: View): Unit = {

    //
    val etDesc = view.findViewById[EditText](R.id.etDesc)
    etDesc addTextChangedListener new EditTextTypeListener(changeByDesc(view))

    //
    val etNote = view.findViewById[EditText](R.id.etNote)
    etNote addTextChangedListener new EditTextTypeListener(changeByNote(view))

    //
    val etExamples = view.findViewById[EditText](R.id.etExamples)
    etExamples addTextChangedListener new EditTextTypeListener(changeByExamples(view))
  }

  protected override def indexUpdated(i: Int, value: MeaningDef, view: View): Unit = resetIndex(view, i + 1)

  protected def resetView(i: Int, meaning: MeaningDef, view: View): Unit = withoutEventCallback {

    resetIndex(view, i + 1)

    //
    val etDesc = view.findViewById[EditText](R.id.etDesc)
    etDesc setText meaning.meaning

    //
    val etNote = view.findViewById[EditText](R.id.etNote)
    etNote setText meaning.note

    //
    val etExamples = view.findViewById[EditText](R.id.etExamples)
    val examplesStr = meaning.examples.mkString("\n")
    etExamples setText examplesStr

    changeBackground(meaning, view)
  }

  // controls here are updated from the code, but it still triggers the listeners
  private def withoutEventCallback(func: => Unit) {

    eventsAreEnabled = false

    func

    eventsAreEnabled = true
  }

  private def isMeaningEmpty(meaningDef: MeaningDef): Boolean = meaningDef.meaning.isEmpty &&
    meaningDef.note.isEmpty &&
    meaningDef.examples.forall(_.isEmpty)

  private def resetIndex(view: View, i: Int): Unit = {

    val tvIndex = view.findViewById[TextView](R.id.tvIndex)
    tvIndex setText s"$i."
  }

  private def resetPlaceholders(): Unit = withoutEventCallback {

    val emptyItems = Range(0, getCount).filter(i => isMeaningEmpty(itemAt(i)))

    if(emptyItems.isEmpty) {

      add(EMPTY)

    } else for(i <- emptyItems.init) {

      remove(i)
    }
  }

  private def updateRecords(itemPanel: View, meaningDef: MeaningDef): Unit = withoutEventCallback {

    set(itemPanel, meaningDef)

    resetPlaceholders()
    changeBackground(meaningDef, itemPanel)
  }

  private def changeBackground(meaningDef: MeaningDef, view: View): Unit = {

    val isEmpty = isMeaningEmpty(meaningDef)
    view.setBackgroundColor(if(isEmpty) Color.GRAY else Color.TRANSPARENT)
  }

  //
  private def changeByDesc(itemPanel: View)(text: String): Unit = if(eventsAreEnabled) {

    val i = indexOf(itemPanel)

    val MeaningDef(_, note, examples) = itemAt(i)
    val meaningDef = MeaningDef(text, note, examples)

    updateRecords(itemPanel, meaningDef)
  }

  private def changeByNote(itemPanel: View)(text: String): Unit = if(eventsAreEnabled) {

    val i = indexOf(itemPanel)

    val MeaningDef(meaning, _, examples) = itemAt(i)
    val meaningDef = MeaningDef(meaning, text, examples)

    updateRecords(itemPanel, meaningDef)
  }

  private def changeByExamples(itemPanel: View)(text: String): Unit = if(eventsAreEnabled) {

    val examples = text.split('\n')
    val i = indexOf(itemPanel)

    val MeaningDef(meaning, note, _) = itemAt(i)
    val meaningDef = MeaningDef(meaning, note, examples)

    updateRecords(itemPanel, meaningDef)
  }

  override def resetItems(items: List[MeaningDef]): Unit = {

    super.resetItems(items :+ EMPTY)
  }
}

