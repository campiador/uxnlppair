/*
    Main.scala
    Purpose: the central client code
    By: Ge Gao & Behnam Heydarshahi
*/

import scala.math._
import breeze.linalg._
import breeze.optimize._

import TopicModel.svd_rank_reduce_and_return_reduced_U_D_Vt

object Main extends App {

  val reviews : List[String] = Crawl.getReviewsFromFile("500")
  // choose k = number of topics here
  val reduced_rank = 10
  val NUMBER_OF_KEYTERMS_PER_TOPIC = 10

  /***********************    matrix on actual data   *************************/ 
  var actual_matrix = TopicModel.term_by_document_matrix(reviews)
  println("**** finished creating term by doc matrix ****")

  var tfidfMatrix = TopicModel.tf_idf_scores(actual_matrix, reviews)
  println("**** finished creating tfid matrix ****")

  // Explanation of SVD: The A is our actual matrix, we call the reduced one "A_reduced"
  val (u, d ,v) = svd_rank_reduce_and_return_reduced_U_D_Vt(tfidfMatrix, reduced_rank)


  // Let's see what are the top terms in each of the topics
  for (topic_number <- 0 until reduced_rank) {
    val key_terms = TopicModel.find_most_common_terms_in_topic(topic_number, NUMBER_OF_KEYTERMS_PER_TOPIC, u)

    println("\nkey terms for topic " + topic_number + ": ")
    key_terms.foreach {println}

  }


}