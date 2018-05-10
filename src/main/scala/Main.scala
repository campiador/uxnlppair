import scala.math._
import breeze.linalg._
import breeze.optimize._

import TopicModel.svd_rank_reduce_and_return_error

object Main extends App {

  val reviews : List[String] = Crawl.getReviewsFromFile("100")
  // choose k = number of topics here
  val reduced_rank = 10

   
  /*******************   Testing term by document matrix   ********************/ 
  var testDocuments = List("the cat sat on my face",
                           "the dog sat on my bed",
                           "the chicken lies on my sheets")
  var matrix = TopicModel.term_by_document_matrix(testDocuments)
  var tfidfMatrix = TopicModel.tf_idf_scores(matrix, testDocuments)
  var str = TopicModel.create_object_for_LSA_model(tfidfMatrix)

  println("term2: ")
  println(TopicModel.getTermByNumber(2))

  println("document3: ")
  println(TopicModel.getDocumentByNumber(2, testDocuments))

  /***********************    matrix on actual data   *************************/ 
  // var actual_matrix = TopicModel.term_by_document_matrix(reviews)
  // println("****finished creating term by doc matrix****")

  // var tfidfMatrix = TopicModel.tf_idf_scores(actual_matrix, reviews)
  // println("****finished creating tfid matrix****")

  // var str = TopicModel.create_object_for_LSA_model(tfidfMatrix)
  // println("****finished creating object for LSA model matrix****")


  // Explanation of SVD: The A is our actual matrix, we call the reduced one "A2"



  // svd_rank_reduce_and_return_error(tfidfMatrix, reduced_rank)



}