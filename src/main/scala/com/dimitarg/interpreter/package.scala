package com.dimitarg

import cats.implicits._
import cats.arrow.FunctionK
import cats.data.{Const, Kleisli, OptionT, State}
import cats.~>
import com.dimitarg.alg.BuildEndpoint
import com.dimitarg.infra.HtReq

package object interpreter {

  type Log[A] = Const[List[String], A]

  type FromReq[A] = Kleisli[OptionT[State[HtReq, *], *], HtReq, A]

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

  def fromReqCompiler: Op ~> FromReq = new FunctionK[Op, FromReq] {

    def consumeFirstSegment: Kleisli[OptionT[State[HtReq, *], *], HtReq, String] = Kleisli { req =>
      req.path match {
        case x::xs => OptionT.liftF(State.set(HtReq(req.methodName, xs, req.query)).map(_ => x))
        case _ => OptionT.none
      }
    }

    override def apply[A](x: Op[A]): FromReq[A] = Kleisli { req =>
      x match {
        case m: MethodName =>
          if (req.methodName == m) {
            OptionT.liftF(State.pure(()))
          } else {
            OptionT.none
          }
        case PathSegment.StringVar(_) =>
          consumeFirstSegment.run(req)
        case PathSegment.Const(pathName) =>
          consumeFirstSegment.run(req).flatMap{segName =>
            if (segName == pathName) {
              OptionT.liftF(State.pure(()))
            } else {
              OptionT.none
            }
          }
        case QueryParam(name) =>
          OptionT.fromOption(req.query.get(name))
      }

    }
  }

  def fromReq[A](prg: BuildEndpoint[A]): HtReq => Option[A] = req => {
    prg
      .foldMap(fromReqCompiler)
      .run(req).value
      .run(req).value
      ._2
  }
}
