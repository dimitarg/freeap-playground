package com.dimitarg

import cats.Applicative
import com.dimitarg.alg.BuildEndpoint
import com.dimitarg.infra.HtReq
import dsl._

object App {

  final case class Input(accountId: String, shrubbery: String)

  def main(args: Array[String]): Unit = {

    val endpoint: BuildEndpoint[Input] = Applicative[BuildEndpoint].map5(
      methodGET,
      constPath("v1"), constPath("accounts"), pathVar("accountId"),
      queryParam("shrubbery")
    ) { (_, _, _, accountId, shrubbery) =>
      Input(accountId, shrubbery)
    }

    val description = interpreter.print(endpoint)

    println(s"description: $description")

    val metricName = interpreter.metricName(endpoint)

    println(s"metric name: $metricName")

    val req = HtReq(
      MethodName.GET,
      List("v1", "accounts", "123452"),
      Map("shrubbery" -> "yes, this is shrubbery")
    )

    val inputFromReq = interpreter.fromReq(endpoint)(req)

    println(inputFromReq)

  }
}
