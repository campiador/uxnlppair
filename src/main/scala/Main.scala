import scala.math._
import breeze.linalg._
import breeze.optimize._

object Main extends App {
  
  Crawl.greetFromCrawl()
  TopicModel.greetFromTopicModel()


  var matrix = TopicModel.term_by_document_matrix(List("The dog chewed my shoes", 
                                                       "The cat scratched my chair",
                                                       "the mouse chewed my cereal"))


  println(matrix)

}