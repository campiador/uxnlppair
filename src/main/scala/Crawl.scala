import java.io.File

import scala.io.Source

object Crawl {

    def greetFromCrawl () {
        println("Greeting from Crawl")
    }

  import net.ruippeixotog.scalascraper.browser.JsoupBrowser
  import net.ruippeixotog.scalascraper.model._

  import net.ruippeixotog.scalascraper.dsl.DSL._
  import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
  import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

  val netflix_review_page_address = "https://play.google.com/store/apps/details?id=com.netflix.mediaclient&hl=en&showAllReviews=true"

  def getReviews():List[String] = {

    val browser = JsoupBrowser()
    val doc = browser.get(netflix_review_page_address)

    // Extract the text inside the element with id "body"
    doc >> text("#body")
    // res2: String = Test page h1

    // Extract the <span> elements inside #review
    val items = doc >> elementList("#review span")
    // items: List[net.ruippeixotog.scalascraper.model.Element] = List(JsoupElement(<span><a href="#home">Home</a></span>), JsoupElement(<span><a href="#section1">Section 1</a></span>), JsoupElement(<span class="active">Section 2</span>), JsoupElement(<span><a href="#section3">Section 3</a></span>))

    // From each item, extract all the text inside their <text> elements
    items.map(_ >> allText("text"))

    List("items not returned properly, use the other function")
  }


  def getReviewsFromFile(number_of_reviews: String) : List[String] = {


    // val original_file = new File(getClass.getClassLoader.getResource("reviews.txt").getPath)

    // val original_lines = Source.fromFile(original_file).getLines()
    val original_lines = Source.fromFile("./src/main/resources/" + number_of_reviews + "reviews.txt").getLines()


    var lines = List[String]()

    for (line <- original_lines) {
//      println(line)
      if (line.length > 1) {
        // lines :+ line
        lines ::= line
      }
    }

    return lines
  }









}