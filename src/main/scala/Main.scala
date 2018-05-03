import scala.math._
import breeze.linalg._
import breeze.optimize._

object Main extends App {

  val reviews : List[String] = Crawl.getReviewsFromFile("1000")


   
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

  // choose k here
  val reduced_rank = 10

  val A = actual_matrix
  val svdA = svd(A)

  val original_rank = rank(A)

  val U2 = svdA.U

  val S = svdA.S
  val S2 = svdA.S(0 to reduced_rank-1)

  val Vt = svdA.Vt
  val Vt2 = svdA.Vt(0 to reduced_rank-1,::)

  val D = diag(svdA.S)
  val D2 = D(::, 0 to reduced_rank-1)

  val A2 = U2 * D2 * Vt2


  // We got the reduced rank matrix, let's calculate the average relative errors for each element
  val diff = A - A2
  val rel_diff = diff / A

  import math.abs

  val rel_diff_abs = rel_diff.map( xi => abs(xi) )


  val sum_rel = sum(rel_diff_abs)
  val avg_rel_error = sum_rel / (original_rank * original_rank)


}