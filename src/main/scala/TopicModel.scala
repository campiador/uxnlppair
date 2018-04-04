/*  
    Three Topics: (1) Responsiveness (UI slow, quick, smooth), 
                 (2) Battery Life (power, shut down)
                 (3) Others

    * Get a corpus of documents where each document is a customer post.
 
    * Stop words: words that we do not want to convert to features because they
                  are not particularly useful.

      We want to clean out these stopwords.


    LSA: 

        input matrix: a matrix where m is the number of documents, and n is
                      is the number of terms

*/


import scala.math._
import breeze.linalg._
import breeze.optimize._

object TopicModel {
    /* type aliasing */
    type Topic = String
    type ReviewPost = String
    type Term = String
    type Occurrence = Int


    // var corpus = List[ReviewPost]
    val topics: List[Topic] = List("RESPONSE", "BATTERY", "OTHER")


    def term_by_document_matrix(corpus: List[ReviewPost]) : DenseMatrix[Occurrence] = {

        /* all the words in the corpus */
        var allTerms = get_all_terms_in_corpus(corpus)

        println("allTerms: " + allTerms.deep.mkString(" "))

        var matrix = DenseMatrix.zeros[Occurrence](corpus.size, allTerms.size)

        for (m <- 0 to (corpus.size-1)) {
            var document = corpus(m)
            var termsInDocument = Util.parsePostBody(document)
            for (n <- 0 to (allTerms.size-1)) {
                var term = allTerms(n)
                matrix(m, n) = termsInDocument.count(_ == term)
            }
        }

        return matrix

    }

    def get_all_terms_in_corpus(corpus: List[ReviewPost]) : Array[Term] = {

        var allTerms = Array[Term]()
        for (document <- corpus) {
            var termsInDocument = Util.parsePostBody(document)
            allTerms = allTerms ++ termsInDocument
        }
        return allTerms.distinct

    }

    def greetFromTopicModel () {
        println("Greeting from TopicModel")
    }

}




