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

  private var indexByView: Map[View, Int] = Map()

  protected  def resetView(i: Int, view: View): Unit = {

    val meaning = itemAt(i)

    resetIndex(view, i+1)

    //
    val etDesc = view.findViewById[EditText](R.id.etDesc)
    etDesc setText meaning.meaning
    etDesc addTextChangedListener new EditTextTypeListener(changeByDesc(view))

    //
    val etNote = view.findViewById[EditText](R.id.etNote)
    etNote setText meaning.note
    etNote addTextChangedListener new EditTextTypeListener(changeByNote(view))

    //
    val etExamples = view.findViewById[EditText](R.id.etExamples)
    val examplesStr = meaning.examples.mkString("\n")
    etExamples setText examplesStr
    etExamples addTextChangedListener new EditTextTypeListener(changeByExamples(view))

    changeBackground(meaning, view)

    indexByView = indexByView + (view -> i)
  }

  private def resetIndex(view: View, i: Int): Unit = {

    val tvIndex = view.findViewById[TextView](R.id.tvIndex)
    tvIndex setText s"$i."
  }

  private def resetPlaceholders(): Unit = {

    val emptyItems = Range(0, getCount).filter(i => isMeaningEmpty(itemAt(i)))

    if(emptyItems.isEmpty) {

      add(EMPTY)

    } else for(i <- emptyItems.init) {

      indexByView = indexByView
        .filter { case (_, j) => j != i }
        .map {
        case (v, j) if j>i => resetIndex(v, j); v -> (j-1)
        case e => e
      }

      remove(i)
    }
  }

  private def isMeaningEmpty(meaningDef: MeaningDef): Boolean = meaningDef.meaning.isEmpty &&
    meaningDef.note.isEmpty &&
    meaningDef.examples.forall(_.isEmpty)

  private def changeBackground(meaningDef: MeaningDef, view: View): Unit = {

    val isEmpty = isMeaningEmpty(meaningDef)
    view.setBackgroundColor(if(isEmpty) Color.GRAY else Color.TRANSPARENT)
  }

  private def changeByDesc(itemPanel: View)(text: String): Unit = {

    val i = indexByView(itemPanel)

    val MeaningDef(_, note, examples) = itemAt(i)
    val meaningDef = MeaningDef(text, note, examples)

    set(i, meaningDef)

    resetPlaceholders()
    changeBackground(meaningDef, itemPanel)
  }

  private def changeByNote(itemPanel: View)(text: String): Unit = {

    val i = indexByView(itemPanel)

    val MeaningDef(meaning, _, examples) = itemAt(i)
    val meaningDef = MeaningDef(meaning, text, examples)

    set(i, meaningDef)

    resetPlaceholders()
    changeBackground(meaningDef, itemPanel)
  }

  private def changeByExamples(itemPanel: View)(text: String): Unit = {

    val examples = text.split('\n')
    val i = indexByView(itemPanel)

    val MeaningDef(meaning, note, _) = itemAt(i)
    val meaningDef = MeaningDef(meaning, note, examples)

    set(i, meaningDef)

    resetPlaceholders()
    changeBackground(meaningDef, itemPanel)
  }

  override def resetItems(items: List[MeaningDef]): Unit = {

    super.resetItems(items :+ EMPTY)
  }
}

