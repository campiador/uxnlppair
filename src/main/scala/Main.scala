import scala.math._
import breeze.linalg._
import breeze.optimize._

object Main extends App {

  val reviews : List[String] = Crawl.getReviewsFromFile()
   
  /*******************   Testing term by document matrix   ********************/ 
  var testDocuments = List("the cat sat on my face", 
                           "the dog sat on my bed")
  var matrix = TopicModel.term_by_document_matrix(testDocuments)
  println(matrix)
  var tfidfMatrix = TopicModel.tf_idf_scores(matrix, testDocuments)
  var str = TopicModel.create_object_for_LSA_model(tfidfMatrix)


  /***********************    matrix on actual data   *************************/ 
  var actual_matrix = TopicModel.term_by_document_matrix(reviews)
  // println(actual_matrix)
 

}