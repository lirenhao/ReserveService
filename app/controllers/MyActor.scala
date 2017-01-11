package controllers

import akka.actor._
import controllers.CmdType.CmdType
import play.api.libs.json._

import scala.collection.mutable

case class Cmd(cmdType: CmdType, cmdUser: String)

object CmdType extends Enumeration {
  type CmdType = Value
  val LOGIN = Value("LOGIN")
  val INIT = Value("INIT")
  val TODO = Value("TODO")
  val DONE = Value("DONE")
  val LOGOUT = Value("LOGOUT")
}

class MyActor extends Actor {
  private val map = mutable.HashMap.empty[String, ActorRef]
  private val set = mutable.LinkedHashSet.empty[String]

  override def receive: Receive = {
    case cmd: Cmd =>
      cmd.cmdType match {
        case CmdType.LOGIN =>
          map += cmd.cmdUser -> sender()
          sender() ! Json.obj("type" -> CmdType.INIT, "payload" -> Json.toJson(set))
        case CmdType.TODO =>
          set += cmd.cmdUser
          sendCmd(cmd)
        case CmdType.DONE =>
          set -= cmd.cmdUser
          sendCmd(cmd)
        case CmdType.LOGOUT =>
          set -= cmd.cmdUser
          map -= cmd.cmdUser
          sendCmd(Cmd(CmdType.DONE, cmd.cmdUser))
        case _ => println
      }
    case _ =>
  }

  def sendCmd(cmd: Cmd): Unit = {
    map.foreach((user) => {
      user._2 ! Json.obj("type" -> cmd.cmdType, "payload" -> cmd.cmdUser)
    })
  }
}

object MyActor {
  private val system = ActorSystem("system")
  val handleActor: ActorRef = system.actorOf(Props[MyActor])
}