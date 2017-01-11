package controllers

import javax.inject.Inject

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.stream.Materializer
import play.api.libs.json.JsValue
import play.api.libs.streams._
import play.api.mvc._

class MyController @Inject()(implicit system: ActorSystem, materializer: Materializer) {

  def socket: WebSocket = WebSocket.accept[JsValue, JsValue] { request =>
    ActorFlow.actorRef(out => WsActor.props(out))
  }
}

object WsActor {
  def props(out: ActorRef) = Props(new WsActor(out))
}

class WsActor(out: ActorRef) extends Actor {

  def receive: PartialFunction[Any, Unit] = {
    case msg: JsValue =>
      MyActor.handleActor.tell(
        Cmd(CmdType.withName((msg \ "type").as[String]), (msg \ "payload").as[String]), out)
    case _ => println
  }
}