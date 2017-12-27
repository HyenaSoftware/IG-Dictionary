package com.hyenawarrior.auxiliary

/**
  * Created by HyenaWarrior on 2017.12.27..
  */
case class TryExtract[A, R](f: A => R) {

  def unapply(arg: A): Option[R] = try {

    Some(f(arg))

  } catch {

    case _: Throwable => None
  }
}
