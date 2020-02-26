package com.dimitarg

import cats.implicits._
import cats.arrow.FunctionK
import cats.data.{Const, Kleisli, StateT}
import cats.~>
import com.dimitarg.alg.BuildEndpoint
import com.dimitarg.infra.HtReq

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


  type FromReq[A] = Kleisli[StateT[Option, List[String], *], HtReq, A]


  def fromReqCompiler: Op ~> FromReq = new FunctionK[Op, FromReq] {

    val consumeFirstSegment: StateT[Option, List[String], String] = StateT { path =>
      path match {
        case x::xs => Some((xs, x))
        case _ => None
      }
    }

    override def apply[A](x: Op[A]): FromReq[A] = Kleisli { req =>
      x match {
        case m: MethodName =>
          if (req.methodName == m) {
            StateT.pure(())
          } else {
            StateT.liftF(None)
          }
        case PathSegment.StringVar(_) =>
          consumeFirstSegment
        case PathSegment.Const(pathName) =>
          consumeFirstSegment.flatMapF { seg =>
            if (seg == pathName) {
              Some(())
            } else {
              None
            }
          }
        case QueryParam(name) =>
          StateT.liftF(req.query.get(name))
      }

    }
  }

  def fromReq[A](prg: BuildEndpoint[A]): HtReq => Option[A] = req => {
    val out =
      prg
      .foldMap(fromReqCompiler)
      .run(req)
      .run(req.path)

    out.flatMap { case (remainingPath, result) =>
      remainingPath match {
        case List() => Some(result)
        case _ => None
      }

    }
  }
}
