package com.wavesplatform.api.grpc

import java.net.InetSocketAddress

import com.typesafe.config.ConfigFactory
import com.wavesplatform.extensions.{Extension, Context => ExtensionContext}
import com.wavesplatform.settings.GRPCSettings
import com.wavesplatform.utils.ScorexLogging
import io.grpc.netty.NettyServerBuilder
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import io.grpc.{Server, ServerBuilder}
import monix.execution.Scheduler

import scala.concurrent.Future

class GRPCServerExtension(context: ExtensionContext) extends Extension with ScorexLogging {
  @volatile
  var server: Server = _

  override def start(): Unit            = {
    val settings = ConfigFactory.load().as[GRPCSettings]("waves.grpc")
    this.server = startServer(settings)
  }

  override def shutdown(): Future[Unit] = {
    log.debug("Shutting down gRPC server")
    if (server != null) {
      server.shutdown()
      Future(server.awaitTermination())(context.actorSystem.dispatcher)
    } else {
      Future.successful(())
    }
  }

  private[this] def startServer(settings: GRPCSettings): Server = {
    implicit val apiScheduler = Scheduler(context.actorSystem.dispatcher)

    val bindAddress = new InetSocketAddress(settings.host, settings.port)
    val server: Server = NettyServerBuilder
      .forAddress(bindAddress)
      .addService(TransactionsApiGrpc.bindService(
        new TransactionsApiGrpcImpl(context.settings.blockchainSettings.functionalitySettings,
                                    context.wallet,
                                    context.blockchain,
                                    context.utx,
                                    context.channels),
        apiScheduler
      ))
      .addService(BlocksApiGrpc.bindService(new BlocksApiGrpcImpl(context.blockchain), apiScheduler))
      .build()
      .start()

    log.info(s"gRPC API was bound to $bindAddress")
    server
  }
}
