package com.akkastrator.state

import java.time.OffsetDateTime

import scala.concurrent.Future

trait WaitProvider {
  def waitUntil(deadline: OffsetDateTime): Future[Unit]
}
