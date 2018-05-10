import scala.math._
import breeze.linalg._
import breeze.optimize._

import TopicModel.svd_rank_reduce_and_return_reduced_U_D_Vt

object Main extends App {

  val reviews : List[String] = Crawl.getReviewsFromFile("100")
  // choose k = number of topics here
  val reduced_rank = 10


   
  /*******************   Testing term by document matrix   ********************/ 
//  var testDocuments = List("the cat sat on my face",
//                           "the dog sat on my bed")
//  var matrix = TopicModel.term_by_document_matrix(testDocuments)
////  println(matrix)
//  var tfidfMatrix = TopicModel.tf_idf_scores(matrix, testDocuments)
//  var str = TopicModel.create_object_for_LSA_model(tfidfMatrix)


  /***********************    matrix on actual data   *************************/ 
  var actual_matrix = TopicModel.term_by_document_matrix(reviews)
  println("****finished creating term by doc matrix****")

  var tfidfMatrix = TopicModel.tf_idf_scores(actual_matrix, reviews)
  println("****finished creating tfid matrix****")

  var str = TopicModel.create_object_for_LSA_model(tfidfMatrix)
  println("****finished creating object for LSA model matrix****")


  // Explanation of SVD: The A is our actual matrix, we call the reduced one "A2"



   // svd_rank_reduce_and_return_reduced_U_D_Vt(tfidfMatrix, reduced_rank)



}