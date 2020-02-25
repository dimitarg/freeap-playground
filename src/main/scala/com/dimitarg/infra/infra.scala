package com.dimitarg

object infra {
  final case class HtReq(methodName: MethodName, path: List[String], query: Map[String, String])
}
