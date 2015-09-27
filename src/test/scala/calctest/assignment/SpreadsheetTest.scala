package calctest.assignment

import org.scalatest._
import Spreadsheet.CellParsers._
import Spreadsheet._
import Spreadsheet.CCT._

class SpreadsheetTest extends FlatSpec with Matchers {
  "cell parser" should "parse A10" in {
     assert(parseAll(cellref, "A10").successful === true)
   }

  "cell parser" should "not parse A01" in {
     parseAll(cellref, "A01").successful should be (false)
   }

   "operator parser" should "parse ++" in {
     parseAll(op, "++").successful should be (true)
   }

   "term parser" should "parse A10 or 7 or -9 or *" in {
     val res1 = parseAll(term, "A10").successful
     val res2 = parseAll(term, "7").successful  
     val res3 = parseAll(term, "-9").successful 
     val res4 = parseAll(term, "*").successful
     println(res1, res2, res3, res4)
     val res = res1 && res2 && res3 && res4
     res should be (true)
   }

   "expr parser" should "parse A10 7 *" in {
     parseAll(expr, "A10 7 *").successful should be (true)
   }

   "expr parser" should "parse A10" in {
     parseAll(expr, "A10").successful should be (true)
   }

   "rowColNumbers" should "read row and col numbers from line" in {
     rowColNumbers("2 2") should be ((2, 2))
   }

   "readExpressions" should "parse n return array of expressions" in {
     val exprs = Array("A10 7 *", "4")
     readExpressions(exprs) should be (Array(List((C, "A10"), (V, "7"), (O, "*")), List((V, "4"))))
   }

   "cellOfIndex(0)" should "evaluate to A1" in {
     cellOfIndex(0, 2, 2) should be ("A1")
   }

   "cellOfIndex(3)" should "evaluate to B2" in {
     cellOfIndex(3, 2, 2) should be ("B2")
   }

   "indexOfCell(B1)" should "evaluate to 2" in {
     indexOfCell("B1", 2, 2) should be (2)
   }

   "readDependencies" should "work fine" in {
     val expressions = readExpressions(Array("B1 7 *", "4", "3", "A2"))
     val dependencyMap = readDependencies(expressions, 2, 2)
     (dependencyMap("A1"), dependencyMap("B2")) should be ((List("B1"), List("A2")))
   }

   "allCells of dependencyMap(A1 -> B1, B2 -> A2)" should "list A1 to B2" in {
     val dependencyMap = Map("A1" -> List("B1"), "B2" -> List("A2"))
     allCells(dependencyMap) should be (Set("A1", "A2", "B1", "B2"))
   }

   "allCells for (2, 2)" should "list A1 to B2" in {
     allCells(2, 2) should be (Set("A1", "A2", "B1", "B2"))
   }

   "revertDependencies for (A1 -> B1, B2 -> A2)" should "evaluate to (B1 -> A1, A2 -> B2)" in {
     val dependencyMap = Map("A1" -> List("B1"), "B2" -> List("A2"), "A2" -> List("B1"))
     val revertMap = revertDependencies(dependencyMap)
     (revertMap("A2"), revertMap("B1")) should be ((List("B2"), List("A1", "A2")))
   }

   "findProcessingOrder without cycles" should "work fine" in {
     val dependencyMap = Map("A1" -> List("B1"), "B2" -> List("A2"), "A2" -> List("B1"))
     findProcessingOrder(dependencyMap, 2, 2) should be (Some(List("B1", "A1", "A2", "B2")))
   }

   "findProcessingOrder with cycles" should "detect cycles" in {
     val dependencyMap = Map("A1" -> List("B1"), "B1" -> List("A2"), "A2" -> List("A1"))
     findProcessingOrder(dependencyMap, 2, 2) should be (None)
   }

   "evaluate 7" should "be just 7" in {
     val expr = List((V, "7")) 
     val resolutions = Map[String, Double]()
     assert((math.abs(evaluate(expr, resolutions)) - 7.0) < 0.0001 === true)
   }

   "evaluate 7 5 + " should "be just 12" in {
     val expr = List((V, "7"), (V, "5"), (O, "+")) 
     val resolutions = Map[String, Double]()
     assert((math.abs(evaluate(expr, resolutions)) - 12.0) < 0.0001 === true)
   }

   "evaluate 7 5 + A1 * with A1 = 5" should "be just 60" in {
     val expr = List((V, "7"), (V, "5"), (O, "+"), (C, "A1"), (O, "*")) 
     val resolutions = Map[String, Double]("A1" -> 5.0)
     assert((math.abs(evaluate(expr, resolutions)) - 60.0) < 0.0001 === true)
   }

   "evaluate 7 5 + A1 * wth A1 = 5" should "be just 60" in {
     val expr =  List((C,"A1"), (C,"B2"), (O,"/"), (V,"2"), (O,"*"))
     val resolutions = Map[String, Double]("B2" -> 3.0, "A2" -> 20.0, "A1" -> 20.0, "A3" -> 20.0)
     assert((math.abs(math.abs(evaluate(expr, resolutions)) - 13.33332)) <= 0.0001 === true)
   }
}
