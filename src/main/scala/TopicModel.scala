/*  
    Three Topics: (1) Responsiveness (UI slow, quick, smooth), 
                  (2) Battery Life (power, shut down)
                  (3) Others

    * Get a corpus of documents where each document is a customer review post.
 
    * Stop words: words that we do not want to convert to features because they
                  are not particularly useful.

                  We want to clean out these stopwords.

    LSA: 
        
        * document-by-term matrix: 
            a matrix X where m is the number of 
            documents, and n is the number of terms

        * single-value-decomposition: 
            X = U*S*Vt

            U: m x k matrix where the rows will be documents and the 
               columns be 'concept'

            S: k x k matrix where the elements are the amount of
               variation captured by each concept

            V: n x k matrix where the rows will be terms and
               columns be 'concept'
        
*/


import scala.math._
import breeze.linalg._
import breeze.optimize._

object TopicModel {
    /* type aliasing */
    type Topic = String
    type ReviewPost = String
    type Term = String
    type Occurrence = Double


    // var corpus = List[ReviewPost]
    val topics: List[Topic] = List("RESPONSE", "BATTERY", "OTHER")




    def create_object_for_LSA_model(dtmatrix : DenseMatrix[Occurrence]) : String = {

        println("\nour document-term-matrix looks like: ")
        println(dtmatrix)
        println("\n")

        val svd.SVD(u,s,v) = svd(dtmatrix)

        println("\nu:")
        println(u)
        println("\ns:")
        println(s)
        println("\nv:")
        println(v)
        println("\n")


        return ""

    }



    // tf(w):  number of times the word appears in a  /
    //         document total number of words in the document
    // idf(w): log(Number of documents) /
    //         Number of document that contains word w
    def tf_idf_scores(dtmatrix : DenseMatrix[Occurrence], corpus: List[ReviewPost]) : DenseMatrix[Occurrence] = {

        var allTerms = get_all_terms_in_corpus(corpus)

        var matrix = DenseMatrix.zeros[Occurrence](corpus.size, allTerms.size)

        for (m <- 0 to dtmatrix.rows-1) {
            var document = dtmatrix(m, ::)            
            var doc = document.t

            var total_words_in_doc = 0.0
            for (n <- 0 to doc.length - 1) { 
                total_words_in_doc = total_words_in_doc + doc(n) 
            }

            for (n <- 0 to doc.length - 1) {
                var num_of_doc_contains_w = 0
                for (m <- 0 to dtmatrix.rows-1) {
                    if (dtmatrix(m, n) != 0) {
                        num_of_doc_contains_w = num_of_doc_contains_w + 1
                    }
                }
                var tf = doc(n) / total_words_in_doc
                var idf = log(corpus.size / num_of_doc_contains_w)
                matrix(m, n) = tf * idf
                // matrix(m, n) = Util.roundToTwoDecimalDigits(tf*idf)
            }

        }

        return matrix

    }


    def term_by_document_matrix(corpus: List[ReviewPost]) : DenseMatrix[Occurrence] = {

        /* all the words in the corpus */
        var allTerms = get_all_terms_in_corpus(corpus).sortWith(_ < _)

        println("allTerms: \n" + allTerms.deep.mkString("  "))

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




