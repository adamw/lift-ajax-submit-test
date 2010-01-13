package com.liftworkshop.snippet

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.java.util.Date
import com.liftworkshop.lib._
import Helpers._
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JsCmd
import xml.{Elem, NodeSeq, Text}
import net.liftweb.http.js.JE.{JsRaw, Str}
import collection.mutable.{ListBuffer, Buffer}
import net.liftweb.http.{S, RequestVar, SHtml, LiftRules}                
import S._
import SHtml._

object CurrentContainer extends RequestVar({ new Container })

class HelloWorld {
  def edit(containerTemplate: NodeSeq): NodeSeq = {
    val cont = CurrentContainer.is

    def reDraw = SetHtml("cont_edit", edit(containerTemplate))

    def editRoom(roomTemplate: NodeSeq): NodeSeq = {
      cont.rooms.flatMap { room: Room =>
          bind("room", roomTemplate,
            "name" -> { println("bind room name " + room.name); SHtml.text(room.name,
              (s: String) => {println("set room name " + s); room.name = s; }) }
            )
      }
    }

    bind("cont", containerTemplate,
      "name" -> { println("bind container name " + cont.name); SHtml.text(cont.name,
              (s: String) => {println("set container name " + s); cont.name = s; }) },
      "rooms" -> editRoom _,
      "addRoom" -> ajaxButton(Text("Add"), "cont_edit",
        () => { println("executing funtion"); cont.rooms += new Room; reDraw })
      )
  }

  def ajaxButton(text: NodeSeq, formId: String, func: () => JsCmd,
                 attrs: (String, String)*): Elem = {
    attrs.foldLeft(fmapFunc(contextFuncBuilder(func))(name =>
            <button onclick={makeAjaxCall(JsRaw(
              LiftRules.jsArtifacts.serialize(formId).toJsCmd + " + " + Str("&" + name + "=true").toJsCmd)).toJsCmd +
                    "; return false;"}>{text}</button>))(_ % _)
  }
}

class Container {
  var name: String = ""
  var rooms = new ListBuffer[Room]
}

class Room {
  var name: String = ""
}


