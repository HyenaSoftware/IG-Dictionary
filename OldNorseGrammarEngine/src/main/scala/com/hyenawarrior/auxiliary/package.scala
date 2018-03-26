package com.hyenawarrior

/**
  * Created by HyenaWarrior on 2017.12.26..
  */
package object auxiliary {

  object & {

    def unapply[T](arg: T): Option[(T, T)] = Some((arg, arg))
  }

  def getCauses(e: Throwable): List[String] = e.getCause match {

    case `e` | null => List(e.getMessage)
    case f => e.getMessage +: getCauses(f)
  }
}
