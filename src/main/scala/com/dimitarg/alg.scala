package com.dimitarg

import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import com.dimitarg.alg.BuildEndpoint

sealed trait Op[A]

sealed trait PathSegment[A] extends Op[A]

object PathSegment {
  final case class Const(value: String) extends PathSegment[Unit]
  final case class StringVar(name: String) extends PathSegment[String]
}

//final case class PathDesc(segments: List[PathSegment])

sealed trait MethodName extends Op[Unit]

object MethodName {
  final case object GET extends MethodName
  final case object POST extends MethodName
}

final case class QueryParam(name: String) extends Op[String]

object alg {
  type BuildEndpoint[A] = FreeApplicative[Op, A]
}

object dsl {
  def constPath(value: String): BuildEndpoint[Unit] = lift(PathSegment.Const(value))
  def pathVar(name: String): BuildEndpoint[String] = lift(PathSegment.StringVar(name))
  val methodGET: BuildEndpoint[Unit] = lift(MethodName.GET)
  val methodPOST: BuildEndpoint[Unit] = lift(MethodName.POST)
  def queryParam(name: String): BuildEndpoint[String] = lift(QueryParam(name))
}