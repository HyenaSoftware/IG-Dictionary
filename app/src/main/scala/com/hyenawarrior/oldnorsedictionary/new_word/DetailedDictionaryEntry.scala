package com.hyenawarrior.oldnorsedictionary.new_word

import android.graphics.drawable.Drawable
import android.os.Bundle
import android.support.v4.content.ContextCompat
import android.support.v7.app.AppCompatActivity
import android.view.View
import android.widget.{LinearLayout, TextView}
import com.hyenawarrior.OldNorseGrammar.grammar.nouns.{Noun, NounStem}
import com.hyenawarrior.OldNorseGrammar.grammar.verbs._
import com.hyenawarrior.OldNorseGrammar.grammar.verbs.enums.VerbClassEnum.{WEAK_A_STEM, WEAK_I_STEM, WEAK_J_STEM}
import com.hyenawarrior.OldNorseGrammar.grammar.{PoSForm, Pos}
import com.hyenawarrior.oldnorsedictionary.model.{DictionaryEntry, DictionaryListItem}
import com.hyenawarrior.oldnorsedictionary.model.database.IGPersister
import com.hyenawarrior.oldnorsedictionary.modelview._
import com.hyenawarrior.oldnorsedictionary.new_word.pages.MeaningDef
import com.hyenawarrior.oldnorsedictionary.{R, modelview}

/**
  * Created by HyenaWarrior on 2018.01.20..
  */
class DetailedDictionaryEntry extends AppCompatActivity {

  private lazy val posViewer = findViewById[LinearLayout](R.id.pos_viewer)

  private var currentItem: DictionaryListItem[_, _ <: PoSForm] = _

  private var activeMeaningAdapter: CustomAdapter[MeaningDef] = _

  protected override def onCreate(savedInstanceState: Bundle): Unit = {

    super.onCreate(savedInstanceState)

    setContentView(R.layout.detailed_dictionary_entry)

    setEditMode(false)

    val serializable = getIntent.getSerializableExtra("entry")

    //
    serializable match {

      case e @ DictionaryListItem(_, _, posObj, meanings) =>
        currentItem = e

        setPosTitle(posObj)

        showMeaning(meanings)

        showWord(posObj)

      case _ => ()
    }
  }

  private def setPosTitle[K, F <: PoSForm](obj: Pos[K, F]): Unit = {

    val tvPosTitle = findViewById[TextView](R.id.tvDetailedViewPoS)

    val title = obj match {

      case StrongVerb(cl, _, _, _, _) => s"${cl.name} verb"
      case WeakVerb(cl, _, _, _) =>

        val wvType = cl match {
          case WEAK_A_STEM => "Type 2"
          case WEAK_J_STEM => "Type 1"
          case WEAK_I_STEM => "Type 3"
        }

        s"${cl.name}/$wvType verb"

      case Noun(NounStem(_, cl), _, _, _) => cl.toString
    }

    tvPosTitle setText title
  }

  private def showMeaning(meanings: List[MeaningDef], writeable: Boolean = false): Unit = {

    val llMeanings = findViewById[LinearLayout](R.id.llMeanings)

    // set meanings
    activeMeaningAdapter =
      if(writeable) new WritableMeaningAdapter(this, llMeanings)
      else new MeaningAdapter(this, llMeanings)

    activeMeaningAdapter resetItems meanings
  }

  private def showWord[K, F <: PoSForm](obj: Pos[K, F]): Unit = {

    val layout = obj match {

      case _: Verb => R.layout.verb_conjugation_viewer_full
      case _: Noun => R.layout.noun_declension_detailed_view
    }

    posViewer.removeAllViews()

    val view = getLayoutInflater.inflate(layout, posViewer)
    modelview.setDeclensionsTo(obj, view)

    val tvWord = findViewById[TextView](R.id.tvWord)
    val priForm = obj.forms
      .get(obj.PRIMARY_KEY)
      .map(_.strRepr)
      .getOrElse("???")

    tvWord.setText(priForm)
  }

  def onCollapseView(view: View): Unit = {

    val (imgViewId, viewPanelId) = view.getId match {

      case R.id.rl_toggle_active_voice_view       => R.id.iv_toggle_active_voice_view        -> R.id.ll_active_voice_view
      case R.id.rl_toggle_mediopassive_voice_view => R.id.iv_toggle_mediopassive_voice_view  -> R.id.ll_mediopassive_voice_view
      case R.id.rl_toggle_past_participle_view    => R.id.iv_toggle_past_participle_view     -> R.id.ll_past_participle_view
    }

    val pic = invertVisibilityAndGetNewPicRsrc(viewPanelId)
    val imgView = findViewById[View](imgViewId)
    imgView setBackground pic
  }

  private def invertVisibilityAndGetNewPicRsrc(viewId: Int): Drawable = {

    val panel = findViewById[View](viewId)
    val (nv, picRsrc) = panel.getVisibility match {

      case View.VISIBLE => View.GONE -> R.drawable.ic_keyboard_arrow_down_black_18dp
      case View.GONE => View.VISIBLE -> R.drawable.ic_keyboard_arrow_up_black_18dp
    }

    panel setVisibility nv

    ContextCompat.getDrawable(getApplicationContext, picRsrc)
  }

  def onEditMeaning(view: View): Unit = {

    setEditMode()

    showMeaning(currentItem.meanings, writeable = true)
  }

  def onSaveEditing(view: View): Unit = {

    setEditMode(false)

    val igPersister = new IGPersister(getApplicationContext)

    //
    val oldDictEntry = DictionaryEntry(currentItem.posObj, currentItem.meanings)
    igPersister.delete(oldDictEntry)

    currentItem = DictionaryListItem(currentItem.otherForms, currentItem.posType, currentItem.posObj, activeMeaningAdapter.allValues)

    // save the current DictionaryEntry
    val newDictEntry = DictionaryEntry(currentItem.posObj, activeMeaningAdapter.allValues)
    igPersister.save(newDictEntry)

    showMeaning(currentItem.meanings)
  }

  def onCancelEditing(view: View): Unit = {

    setEditMode(false)

    showMeaning(currentItem.meanings)
  }

  private def setEditMode(enabled: Boolean = true): Unit = {

    val otherVisibility = if(enabled) View.VISIBLE else View.GONE
    val editVisibility = if(enabled) View.INVISIBLE else View.VISIBLE

    findViewById[View](R.id.ibEdit) setVisibility editVisibility
    findViewById[View](R.id.ibSave) setVisibility otherVisibility
    findViewById[View](R.id.ibCancel) setVisibility otherVisibility
  }
}
