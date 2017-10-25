package com.wavesplatform.discovery

import java.util.concurrent.TimeUnit

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.TextMessage
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.wavesplatform.discovery.actors.MainActor
import com.wavesplatform.discovery.actors.MainActor.WebSocketConnected

import scala.concurrent.duration.FiniteDuration
import scala.io.StdIn

object DiscoveryApp extends App {
  implicit val system: ActorSystem = ActorSystem("Default")
  implicit val flowMaterializer: ActorMaterializer = ActorMaterializer()

  val mainActor = MainActor(Settings.default.workersCount)

  mainActor ! MainActor.Peers(Settings.default.initialPeers.toSet)

  system.scheduler.schedule(FiniteDuration(0, TimeUnit.SECONDS),FiniteDuration(500, TimeUnit.MILLISECONDS), mainActor, MainActor.Discover)

  import akka.http.scaladsl.server.Directives._

  val route = get {
    pathEndOrSingleSlash {

      val sink: Sink[akka.http.scaladsl.model.ws.Message, _] =  Sink.ignore
      val source: Source[akka.http.scaladsl.model.ws.Message, NotUsed] =
        Source.actorRef[String](1, OverflowStrategy.dropTail)
          .mapMaterializedValue { actor =>
            mainActor ! WebSocketConnected(actor)
            NotUsed
          }.map(
          (outMsg: String) => TextMessage(outMsg))

      handleWebSocketMessages(Flow.fromSinkAndSource(sink, source))
    }
  }

  val binding = Http().bindAndHandle(route, Settings.default.webSocketHost, Settings.default.webSocketPort)
  println(s"Server is now online at http://${Settings.default.webSocketHost}:${Settings.default.webSocketPort}\nPress RETURN to stop...")
  StdIn.readLine()
  binding.flatMap(_.unbind()).onComplete(_ => {
    system.terminate()
    workerGroup.shutdownGracefully()
  })
}
