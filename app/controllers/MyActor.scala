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
  private val map = mutable.HashMap.empty[String, mutable.Set[ActorRef]]
  private val set = mutable.LinkedHashSet.empty[String]

  override def receive: Receive = {
    case cmd: Cmd =>
      cmd.cmdType match {
        case CmdType.LOGIN =>
          context.watch(sender())
          map.get(cmd.cmdUser) match {
            case Some(userSet) =>
              map.update(cmd.cmdUser, userSet += sender())
            case None =>
              map += cmd.cmdUser -> mutable.Set(sender())
          }
          sender() ! Json.obj("type" -> CmdType.INIT, "payload" -> Json.toJson(set))
        case CmdType.TODO =>
          set += cmd.cmdUser
          sendCmd(cmd)
        case CmdType.DONE =>
          set -= cmd.cmdUser
          sendCmd(cmd)
        case CmdType.LOGOUT =>
          context.unwatch(sender())
          map.get(cmd.cmdUser) match {
            case Some(userSet) =>
              if (userSet.size > 1)
                map.update(cmd.cmdUser, userSet -= sender())
              else {
                set -= cmd.cmdUser
                map -= cmd.cmdUser
                sendCmd(Cmd(CmdType.DONE, cmd.cmdUser))
              }
            case None =>
          }
        case _ => println
      }
    case Terminated(ref) =>
      map.keys
        .filter((user) => map(user) == ref)
        .foreach((user) => {
          map.get(user) match {
            case Some(userSet) =>
              if (userSet.size > 1)
                map.update(user, userSet -= sender())
              else {
                set -= user
                map -= user
                sendCmd(Cmd(CmdType.DONE, user))
              }
            case None =>
          }
        })
    case _ =>
  }

  def sendCmd(cmd: Cmd): Unit = {
    map.values.foreach((user) =>
      user.foreach((ref) =>
        ref ! Json.obj("type" -> cmd.cmdType, "payload" -> cmd.cmdUser))
    )
  }
}

object MyActor {
  private val system = ActorSystem("system")
  val handleActor: ActorRef = system.actorOf(Props[MyActor])
}