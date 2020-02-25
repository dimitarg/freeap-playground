package com.dimitarg

import cats.implicits._
import dsl._

object App {
  def main(args: Array[String]): Unit = {

    val endpoint = methodGET *>
      constPath("v1") *> constPath("accounts")  *> pathVar("accountId") *>
        queryParam("shrubbery")

    val description = interpreter.print(endpoint)

    println(s"description: $description")

    val metricName = interpreter.metricName(endpoint)

    println(s"metric name: $metricName")

  }
}
