

import scala.io.Source._
import scala.collection.mutable.HashMap
import org.apache.logging.log4j.LogManager

object Day7 {

  val log = LogManager.getLogger("org.github.adventofcode")

  class Input
  case class Wire(var name: String, var input: Option[Gate] = None, var solved : Boolean = false, var value : Char = 0) extends Input
  case class Signal(var value: Char = 0) extends Input

  abstract class Gate(var inputs: List[Input], var output: Wire)
  class AndGate(inputs: List[Input], output: Wire) extends Gate(inputs, output)
  class OrGate(inputs: List[Input], output: Wire) extends Gate(inputs, output)
  class RShiftGate(inputs: List[Input], output: Wire) extends Gate(inputs, output)
  class LShiftGate(inputs: List[Input], output: Wire) extends Gate(inputs, output)
  class NotGate(inputs: List[Input], output: Wire) extends Gate(inputs, output)
  class PassGate(inputs: List[Input], output: Wire) extends Gate(inputs, output)

  var circuit = HashMap[String, Wire]()

  def main(args: Array[String]): Unit = {
    log.info("day7")

    var lines = fromFile("./src/day7.txt").getLines.toList

    for (line <- lines) {

      // log.infoln(line)

      // split by arrow
      var parts = line.split("->")

      val wname = parts.last.trim
      val wire = getWire(wname)

      // split the lhs
      val lhs = parts.head.split(' ')

      lhs.size match {
        case 1 => {
          // this is a signal or wire
          safeToInt(lhs(0)) match {
            case Some(i) => {
              val s = new Signal(lhs(0).toInt.toChar)
              val p = new PassGate(List(s), wire)
              wire.input = Some(p)
            }
            case None => {
              val w = getWire(lhs(0))
              val p = new PassGate(List(w), wire)
              wire.input = Some(p)
            }
          }
        }
        case 2 => {
          // not gate
          safeToInt(lhs(1)) match {
            case Some(i) => {
              val s = new Signal(i)
              log.info("bust")
            }
            case None => {

              val w = getWire(lhs(1))
              val n = new NotGate(List(w), wire)
              wire.input = Some(n)

            }
          }

        }
        case 3 => {
          // and gate
          // or gate
          // shift gate

          // the first item should be a number or wire
          val left = signalOrWire(lhs(0))

          // the last item should be a number or wire
          val right = signalOrWire(lhs(2))

          // the middle item should be an operator
          lhs(1) match {
            case "OR" => {
              val or = new OrGate(List(left, right), wire)
              wire.input = Some(or)
            }
            case "AND" => {
              val and = new AndGate(List(left, right), wire)
              wire.input = Some(and)
            }
            case "RSHIFT" => {
              val r = new RShiftGate(List(left, right), wire)
              wire.input = Some(r)
            }
            case "LSHIFT" => {
              val l = new LShiftGate(List(left, right), wire)
              wire.input = Some(l)
            }
            case _ => {
              log.info(lhs(1) + " " + line)
            }
          }
        }
        case _ => {
          log.info(line)
        }
      }

    }

    log.info("no of wires:" + circuit.size)

    val wirea = getWire("a")

    log.info(wirea)

    // walk(wirea, 0)

    log.info( evaluate(wirea).toInt )
  }

  def signalOrWire(src: String): Input = {
    safeToInt(src) match {
      case Some(i) => {
        new Signal(i)
      }
      case None => {
        getWire(src)
      }
    }
  }

  def safeToInt(src: String): Option[Char] = {

    try {
      Some(src.toInt.toChar)
    } catch {
      case e: Exception => {
        None
      }
    }

  }

  def getWire(name: String): Wire = {

    circuit.get(name) match {
      case Some(w) => { w }
      case None => {
        val wire = new Wire(name)
        circuit.put(name, wire)
        wire
      }
    }
  }

  def evaluate(input: Input): Char = {

    input match {
      case w: Wire => {
        log.info("eval:" + w.name)
        if (!w.solved ) {
          w.input match {
            case Some(g) => { w.value = evaluate(g) }
            case _ => { log.info("wtf") }
          }
          w.solved = true
        }
        w.value
      }
      case s: Signal => {
        Console.println("signal")
        s.value }
    }

  }

  def evaluate(gate: Gate): Char = {

    gate match {

      case a: AndGate => {
        log.info("and")
        val l = evaluate(a.inputs(0))
        val r = evaluate(a.inputs(1))
        (l.&(r)).toChar
      }
      case o: OrGate => {
        log.info("or")
        val l = evaluate(o.inputs(0))
        val r = evaluate(o.inputs(1))
        (l.|(r)).toChar
      }
      case rs: RShiftGate => {
        log.info("rshift")
        val l = evaluate(rs.inputs(0))
        val r = evaluate(rs.inputs(1))
        (l.>>(r)).toChar
      }
      case ls: LShiftGate => {
        log.info("lshift")
        val l = evaluate(ls.inputs(0))
        val r = evaluate(ls.inputs(1))
        (l.<<(r)).toChar
      }
      case n: NotGate => {
        log.info("not")
        val r = evaluate(n.inputs(0))
        (Char.MaxValue - r).toChar
      }
      case p: PassGate => {
        log.info("pass")
        evaluate(p.inputs(0))
      }
    }
  }

  def walk(obj: Any, indent: Int): Unit = {

    obj match {
      case s: Signal => {
        log.info(printIndent(indent) + s.value)
      }
      case w: Wire => {
        log.info(printIndent(indent) + "(w:" + w.name)
        walk(w.input.get, indent + 1)
        printIndent(indent)
        log.info(printIndent(indent) + ")")
      }
      case a: AndGate => {
        log.info(printIndent(indent) + "(and:")
        walk(a.inputs(0), indent + 1)
        walk(a.inputs(1), indent + 1)
        printIndent(indent)
        log.info(printIndent(indent) + ")")
      }
      case o: OrGate => {
        log.info(printIndent(indent) + "(or:")
        walk(o.inputs(0), indent + 1)
        walk(o.inputs(1), indent + 1)
        printIndent(indent)
        log.info(printIndent(indent) + ")")
      }
      case r: RShiftGate => {
        log.info(printIndent(indent) + "(rshift:")
        walk(r.inputs(0), indent + 1)
        walk(r.inputs(1), indent + 1)
        printIndent(indent)
        log.info(printIndent(indent) + ")")
      }
      case l: LShiftGate => {
        log.info(printIndent(indent) + "(lshift:")
        walk(l.inputs(0), indent + 1)
        walk(l.inputs(1), indent + 1)
        printIndent(indent)
        log.info(printIndent(indent) + ")")
      }
      case n: NotGate => {
        log.info(printIndent(indent) + "(not:")
        walk(n.inputs(0), indent + 1)
        printIndent(indent)
        log.info(printIndent(indent) + ")")
      }
      case p: PassGate => {
        log.info(printIndent(indent) + "(pass:")
        walk(p.inputs(0), indent + 1)
        printIndent(indent)
        log.info(printIndent(indent) + ")")
      }
    }

  }

  def printIndent(i: Int): String = {
    var indent = ""
    for (j <- 0 to i) {
      indent = indent + " "
    }

    indent
  }

}