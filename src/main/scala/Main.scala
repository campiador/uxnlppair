import scala.math._
import breeze.linalg._
import breeze.optimize._

import TopicModel.svd_rank_reduce_and_return_reduced_U_D_Vt

object Main extends App {

  val reviews : List[String] = Crawl.getReviewsFromFile("500")
  // choose k = number of topics here
  val reduced_rank = 10
  val NUMBER_OF_KEYTERMS_PER_TOPIC = 10


   
  /*******************   Testing term by document matrix   ********************/ 
//  var testDocuments = List("the cat sat on my face",
//                           "the dog sat on my bed")
//  var matrix = TopicModel.term_by_document_matrix(testDocuments)
////  println(matrix)
//  var tfidfMatrix = TopicModel.tf_idf_scores(matrix, testDocuments)
//  var str = TopicModel.create_object_for_LSA_model(tfidfMatrix)


  /***********************    matrix on actual data   *************************/ 
  var actual_matrix = TopicModel.term_by_document_matrix(reviews)
  println("**** finished creating term by doc matrix ****")

  var tfidfMatrix = TopicModel.tf_idf_scores(actual_matrix, reviews)
  println("**** finished creating tfid matrix ****")

  var str = TopicModel.create_object_for_LSA_model(tfidfMatrix)
  println("**** finished creating object for LSA model matrix ****")


  // Explanation of SVD: The A is our actual matrix, we call the reduced one "A2"




  val (u, d ,v) = svd_rank_reduce_and_return_reduced_U_D_Vt(tfidfMatrix, reduced_rank)



//  val top_terms_for_model = TopicModel.find_most_common_terms_in_topic(3, 10, u)

  for (topic_number <- 0 until reduced_rank) {
    val key_terms = TopicModel.find_most_common_terms_in_topic(topic_number, NUMBER_OF_KEYTERMS_PER_TOPIC, u)

    println("\nkey terms for topic " + topic_number + ": ")
    key_terms.foreach {println}

  }




}