import java.awt.Color
import scala.swing._
import scala.swing.event._


object main extends App {
  val collection = new Spline
  val mx = 20
  for (i <- 0 until mx) {
    val portion = (Math.PI * 2.0 / mx)
    var angle = portion * i
    if (i % 2 == 0) {
      angle = angle + portion * 0.2
    } else {
      angle = angle - portion * 0.2

    }
    val offset = Vec2(300, 300)
    val radius = 200.0f
    val anchorDist = 50.0f
    val node = collection.addNode(Node(
      position = offset + Vec2(Math.cos(angle), Math.sin(angle)) * radius,
      anchor1 = Vec2(
        Math.cos(angle + Math.PI) * anchorDist,
        Math.sin(angle + Math.PI) * anchorDist,
      ),
      anchor2 = Vec2(
        Math.cos(angle) * anchorDist * 0.2,
        Math.sin(angle) * anchorDist * 0.2,
      ),
    ))
    if (i % 2 == 0) {
      node.anchors = node.anchors.reverse
    }
  }
  lazy val mainPanel: Panel = new Panel() {
    focusable = true
    background = Color.white
    preferredSize = new Dimension(600, 600)
    listenTo(mouse.clicks, mouse.moves)

    reactions += {
      case e: MousePressed => {
        collection.update(
          MouseContext(
            mouseState = MouseStateEnum.Down, mousePosition = Vec2(e.point.x, e.point.y)
          )
        )
        repaint()
      }
      case e: MouseReleased => {
        collection.update(
          MouseContext(
            mouseState = MouseStateEnum.Up, mousePosition = Vec2(e.point.x, e.point.y)
          )
        )
        repaint()
      }
      case e: MouseDragged => {
        collection.update(
          MouseContext(
            mouseState = MouseStateEnum.Move, mousePosition = Vec2(e.point.x, e.point.y)
          )
        )
        repaint()
      }
    }


    override def paint(g: Graphics2D): Unit = {
      super.paint(g)
      collection.paint(g)
    }
  }

  var btn = new Button {
    text = "Closed/Open"
  }
  var btn2 = new Button {
    text = "Lock Bezier/Unlock"
  }
  val frame: Frame = new MainFrame {
    title = "Hello World"


    btn.reactions += {
      case e: ButtonClicked => {
        collection.closed = !collection.closed
        repaint()
      }
    }


    btn2.reactions += {
      case e: ButtonClicked => {
        collection.nodes.find(_.isSelected) match {
          case Some(value) => value.locked = !value.locked
          case None =>
        }
        repaint()
      }
    }

    contents = new FlowPanel {
      background = Color.white
      preferredSize = new Dimension(600, 700)
      contents += mainPanel += btn += btn2
    }
  }

  frame.centerOnScreen()
  frame.open()
}