package calctest.assignment

import scala.util.parsing.combinator._

object Spreadsheet {

  // Cell contents consist of these: Cell reference, Operator, or positive decimal value.
  object CCT extends Enumeration { type CCT = Value; val C, O, V = Value }
  import CCT._

  type CellExpr = List[(CCT.Value, String)]

  object CellParsers extends JavaTokenParsers {

    def cellref: Parser[String] = """[A-Z][1-9]\d*""".r 

    def op: Parser[String] = "++" | "--" | "+" | "-" | "*" | "/" 

    def term: Parser[(CCT.Value, String)] = 
      floatingPointNumber ^^ { (V, _) } | 
      op                  ^^ { (O, _) } | 
      cellref             ^^ { (C, _) }

    def expr: Parser[CellExpr] = term ~ rep(term) ^^ { case a ~ list => List(a) ++ list }
  }
  import CellParsers._

  def rowColNumbers(line: String): (Int, Int) = { 
    val Array(r, c) = line.split(' '); (r.toInt, c.toInt) 
  }

  def readExpressions(lines: Array[String]): Array[CellExpr] = {
    lines.map(parseAll(expr, _).get)
  }

  def cellOfIndex(i: Int, rowN: Int, colN: Int): String = {
    val row = i / colN
    val col = i % colN + 1
    ('A' + row).toChar.toString + col.toString
  }

  def indexOfCell(cell: String, rowN: Int, colN: Int): Int = {
    val row:Int = cell.head - 'A'
    val col:Int = cell.tail.toInt - 1
    colN * row + col
  }

  def readDependencies(expressions: Array[CellExpr], rowN: Int, colN: Int): Map[String, List[String]] = {
    expressions.zipWithIndex.map{ case (a, b) => 
      cellOfIndex(b, rowN, colN) -> a.filter(_._1 == C).map(_._2) 
    } 
    .toMap
  }

  def allCells(cellMap: Map[String, List[String]]): Set[String] =
    (cellMap.keys ++ cellMap.values.flatMap(identity)).toSet

  def allCells(rowN: Int, colN: Int): Set[String] = {
    (0 until rowN * colN).map(cellOfIndex(_, rowN, colN)).toSet
  }

  def revertDependencies(dMap: Map[String, List[String]]): Map[String, List[String]] = {
    dMap.toList
        .flatMap{ x => Array.fill(x._2.size)(x._1).zip(x._2) }
        .map(_.swap)
        .groupBy(_._1)
        .mapValues(_.map(_._2))
  }

  def findProcessingOrder(dependencyMap: Map[String, List[String]], rowN: Int, colN: Int): Option[List[String]] = {
    val reverseMap = revertDependencies(dependencyMap)
    val nodependencyCells = reverseMap.keys.toSet -- dependencyMap.keys
    var set = nodependencyCells
    var list = List.empty[String]
    var edges = reverseMap.toList.flatMap{ x => Array.fill(x._2.size)(x._1).zip(x._2) }.toSet
    while (set.nonEmpty) {
      val n = set.head
      set = set - n
      list = n :: list
      reverseMap.get(n).foreach { l => 
        l.map { m =>
          edges = edges - ((n, m)) 
          if (!edges.map(_._2).toSet(m)) set = set + m
        }
      }
    }
    if (edges.nonEmpty) None else Some(list.reverse)
  }

  def evaluate(expr: CellExpr, resolutions: Map[String, Double]): Double = {
    var stack = List.empty[Double]
    expr.foreach { 
      case (C, c) => stack = resolutions(c) :: stack
      case (V, v) => stack = v.toDouble :: stack
      case (O, o) => {
        if (o == "++") stack = (stack.head + 1) :: stack.tail
        else if (o == "--") stack = (stack.head - 1) :: stack.tail
        else stack match {
          case x :: y :: xs => if (o == "+")      stack = (y + x) :: stack.drop(2)
                               else if (o == "-") stack = (y - x) :: stack.drop(2)
                               else if (o == "*") stack = (y * x) :: stack.drop(2)
                               else if (o == "/") stack = (if (x != 0) (y / x) else 0.0f)  :: stack.drop(2)
          case _ => 
        }
      }
    }
    assert (stack.size == 1) 
    stack.head
  }

  def main(args: Array[String]): Unit = {
    val lines = io.Source.stdin.getLines.toArray
    val (colN, rowN) = rowColNumbers(lines.head)
    val expressions = readExpressions(lines.tail)
    val dependencyMap = readDependencies(expressions, rowN, colN)
    val processingOrder = findProcessingOrder(dependencyMap, rowN, colN)
    if (processingOrder.isEmpty) {
      println("Spreadsheet cells have cyclical dependency") 
      sys.exit(1)
    }
    val cells = allCells(rowN, colN)
    val dependencyOrder = processingOrder.get
    val nodependencyCells = cells -- dependencyOrder
    val order = nodependencyCells.toList ++ dependencyOrder
    var resolutions = Map[String, Double]()
    order.foreach{ cell =>
      val index = indexOfCell(cell, rowN, colN)
      val expr  = expressions(index)
      val result = evaluate(expr, resolutions)
      resolutions = resolutions + (cell -> result)
    }

    val result = resolutions.toList.map{ case (k, v) => indexOfCell(k, rowN, colN) -> "%.5f".format(v) }.sortBy(_._1).map(_._2).mkString("\n")
    println(result)
  }
}
