import java.awt.Color
import scala.collection.mutable.ArrayBuffer
import scala.swing.{Graphics2D, Point}

sealed trait NodeType {
  def id: Int
}

case object NodeTypeEnum {
  case object Linear extends NodeType {
    val id = 0
  }

  case object Bezier extends NodeType {
    val id = 1
  }
}

sealed trait MouseState {
  def id: Int
}

case object MouseStateEnum {
  case object Down extends MouseState {
    val id = 0
  }

  case object Move extends MouseState {
    val id = 1
  }

  case object Up extends MouseState {
    val id = 2
  }
}

case class MouseContext(mouseState: MouseState, mousePosition: Vec2)


trait Updatable {
  def update(context: MouseContext): Boolean
}

trait Drawable {
  def paint(g: Graphics2D): Unit
}

case class Vec2(var x: Double, var y: Double) {
  def distance(other: Vec2): Double = Math.sqrt(
    Math.pow(other.x - x, 2) + Math.pow(other.y - y, 2)
  ).toFloat

  def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)

  def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)


  def *(other: Double): Vec2 = Vec2(x * other, y * other)


  def angle(other: Vec2): Double = {
    val diff = this - other
    Math.atan2(diff.y, diff.x)
  }

  def toPoint: Point = new Point(x.toInt, y.toInt)
}

case class Node(var position: Vec2, anchor1: Vec2, anchor2: Vec2) extends Updatable with Drawable {
  private val size = 6
  private val anchorSize = 4
  private var hitTest = false
  private var lastPos: Vec2 = _
  private var lastMouse: Vec2 = _

  var isSelected = false

  var nodeType: NodeType = NodeTypeEnum.Bezier
  var locked = true

  var parent: Spline = _


  private var hitTestAnchor = false
  private var selectedAnchor: Int = -1
  var anchors = new Array[Vec2](_length = 2)
  anchors(0) = anchor1
  anchors(1) = anchor2

  override def update(context: MouseContext): Boolean = {
    context.mouseState match {
      case MouseStateEnum.Down => {

        for (i <- anchors.indices) {
          val anchor = anchors(i)
          if (context.mousePosition.distance(anchor + position) < size) {
            selectedAnchor = i
            hitTestAnchor = true
            lastMouse = context.mousePosition
            lastPos = anchor
            return true
          }
        }

        if (context.mousePosition.distance(position) < size) {
          hitTest = true
          lastMouse = context.mousePosition
          lastPos = position
          parent.nodes.foreach {
            _.isSelected = false
          }
          isSelected = true
          return true
        }
      }
      case MouseStateEnum.Move => {
        if (hitTestAnchor) {

          val nwp = lastPos - (lastMouse - context.mousePosition)
          if (!locked) {
            anchors(selectedAnchor) = nwp
            return true
          }

          val angle = Math.atan2(nwp.y, nwp.x)
          val dist1 = nwp.distance(Vec2(0, 0))
          val dist2 = anchors(1 - selectedAnchor).distance(Vec2(0, 0))
          anchors(selectedAnchor) = Vec2(
            Math.cos(angle) * dist1,
            Math.sin(angle) * dist1,
          )
          anchors(1 - selectedAnchor) = Vec2(
            Math.cos(angle + Math.PI) * dist2,
            Math.sin(angle + Math.PI) * dist2,
          )
          return true
        }
        if (hitTest) {
          position = lastPos - (lastMouse - context.mousePosition)
          return true
        }
      }
      case MouseStateEnum.Up => {
        hitTest = false
        hitTestAnchor = false
      }
    }
    false
  }

  override def paint(g: Graphics2D): Unit = {
    g.setColor(if (isSelected) Color.red else Color.black)
    g.fillOval(position.x.toInt - size / 2, position.y.toInt - size / 2, size, size)

    anchors.zipWithIndex.foreach {
      case (v, i) => {
        g.setColor(if (selectedAnchor == i) Color.orange else Color.gray)
        g.fillOval((position.x + v.x).toInt - anchorSize / 2, (position.y + v.y).toInt - anchorSize / 2, anchorSize, anchorSize)
      }
    }
  }
}


class Spline extends Updatable with Drawable {
  var nodes: ArrayBuffer[Node] = new ArrayBuffer[Node]

  var closed = true

  def smooth(a: Double, b: Double, p: Double): Double = a + (b - a) * p

  def addNode(node: Node): Node = {
    nodes += node
    node.parent = this
    node
  }

  override def update(context: MouseContext): Boolean = {
    nodes.find(
      _.update(context)
    )
    if (context.mouseState == MouseStateEnum.Up) {
      println(nodes.map(_.position))
    }
    false
  }


  def calcPoint(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2, t: Double): Vec2 = {
    val a1: Vec2 = p1 * (Math.pow(1 - t, 3))
    val a2: Vec2 = p2 * (3 * Math.pow(1 - t, 2) * t)
    val a3: Vec2 = p3 * (3 * (1 - t) * Math.pow(t, 2))
    val a4: Vec2 = p4 * Math.pow(t, 3)
    a1 + a2 + a3 + a4
  }

  def calcSpline(): ArrayBuffer[Vec2] = {
    val po = new ArrayBuffer[Vec2]()
    val controlObjects = new ArrayBuffer[Vec2]()

    for (i <- nodes.indices) {
      val node = nodes(i)
      if (i != 0)
        controlObjects += node.position + node.anchors.head
      controlObjects += node.position
      if (i != nodes.length - 1)
        controlObjects += node.position + node.anchors.last
    }

    if (closed) {
      controlObjects += nodes.last.position + nodes.last.anchors.last
      controlObjects += nodes.head.position + nodes.head.anchors.head
      controlObjects += nodes.head.position
    }
    var i = 0

    while (i < controlObjects.length - 1) {
      val a = controlObjects(i)
      val b = controlObjects(i + 1)
      val c = controlObjects(i + 2)
      val d = controlObjects(i + 3)


      var t: Double = 0
      while (t < 1) {
        po += calcPoint(a, b, c, d, t)
        t = t + 0.1
      }
      i = i + 3
    }
    po
  }

  override def paint(g: Graphics2D): Unit = {
    nodes.foreach(_.paint(g))
    g.setColor(Color.blue)

    val lst = calcSpline()
    for (i <- 0 until lst.length - 1) {
      val a = lst(i)
      val b = lst(i + 1)
      g.drawLine(a.x.toInt, a.y.toInt, b.x.toInt, b.y.toInt)

    }
  }
}