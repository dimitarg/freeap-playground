package com.dimitarg

import cats.implicits._
import cats.arrow.FunctionK
import cats.data.Const
import cats.~>
import com.dimitarg.alg.BuildEndpoint

package object interpreter {
  type Log[A] = Const[List[String], A]

  def printerCompiler: Op ~> Log = new FunctionK[Op, Log] {
    override def apply[A](x: Op[A]): Log[A] = {
      Const(List(x.toString()))
    }
  }

  def print[A](prg: BuildEndpoint[A]): String = prg.foldMap(printerCompiler).getConst.mkString(" ")

  def metricsNameCompiler: Op ~> Log = new FunctionK[Op, Log] {
    override def apply[A](x: Op[A]): Log[A] = x match {
      case PathSegment.Const(x) => Const(List(x))
      case PathSegment.StringVar(x) => Const(List(s"<${x}>"))
      case _ => Const(List())
    }
  }

  def metricName[A](prg: BuildEndpoint[A]): String = prg.foldMap(metricsNameCompiler).getConst.mkString("/")
}
