package com.hyenawarrior.OldNorseGrammar.grammar.calcinfra.metrics

import com.hyenawarrior.OldNorseGrammar.grammar.phonology.{Consonant, Phoneme, Vowel2}

/**
  * Created by HyenaWarrior on 2018.11.17..
  */
object Metrics {

  def compactnessOf(phonemes: Seq[Phoneme]): Int = {

    /* hvasas, hvass */
    val (_, buckets) = phonemes.foldLeft((0x3: Byte) -> List(0)) {

      case ((c, h :: t), ph) if (typeOf(ph) & c) != 0 =>
        (typeOf(ph), (h + 1) :: t)

      case ((_, l), ph) =>
        (typeOf(ph), 1 :: l)
    }

    val avgBucketWeight = buckets.sum.toFloat / buckets.length.toFloat

    (avgBucketWeight * 100).toInt
  }

  private def typeOf(ph: Phoneme): Byte = ph match {

    case _: Vowel2 => 0x1
    case _: Consonant => 0x2
    case _ => 0
  }
}
