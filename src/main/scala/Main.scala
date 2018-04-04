import scala.math._
import breeze.linalg._
import breeze.optimize._

object Main extends App {
  
  /*******************   Testing term by document matrix   ********************/ 
  var testDocuments = List("The dog chewed my shoes", 
                           "The cat scratched my chair",
                           "the mouse chewed my cereal")
  var matrix = TopicModel.term_by_document_matrix(testDocuments)
  println(matrix)


  
}