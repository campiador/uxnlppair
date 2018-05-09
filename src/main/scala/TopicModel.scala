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



import TopicModel.Occurrence

import scala.math._
import breeze.linalg.{DenseMatrix, _}
import breeze.optimize._

import Util.parsePostBody

object TopicModel {
    /* type aliasing */
    type Topic = String
    type ReviewPost = String
    type Term = String
    type Occurrence = Double


    // var corpus = List[ReviewPost]
    val topics: List[Topic] = List("RESPONSE", "BATTERY", "OTHER")


    val stopwordsFile = scala.io.Source.fromFile("./src/main/resources/nltk_stop_words.txt")

    var stopwords: List[String] = List() 

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

        println("\n\n\nallTerms: \n\n" + allTerms.deep.mkString("  "))

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

        for (line <- stopwordsFile.getLines) {
            stopwords = stopwords :+ line.toLowerCase
        }

        var allTerms = Array[Term]()
        for (document <- corpus) {
            var termsInDocument = Util.parsePostBody(document)
            var cleaned_termsInDocument = termsInDocument.filterNot(stopwords.contains(_))
            allTerms = allTerms ++ cleaned_termsInDocument
        }
        return allTerms.distinct

    }

    def greetFromTopicModel() {
        println("Greeting from TopicModel")
    }


//    Code to reduce the rank of a matrix through SVD decomposition
    def svd_rank_reduce_and_return_error(actual_matrix: DenseMatrix[Occurrence], reduced_rank: Int)  = {
        val A = actual_matrix
        val svdA = svd(A)

        val original_rank = rank(A)
        println("rank: " +  original_rank  + ", cols: "+ A.cols + ", rows:"  + A.rows +
          ", A(0,0): " + A(0, 0) + ", A(98,0): " + A(98, 0))


        sys.exit(2)

        val U2 = svdA.U

        val S = svdA.S
        val S2 = svdA.S(0 to reduced_rank - 1)

        val Vt = svdA.Vt
        val Vt2 = svdA.Vt(0 to reduced_rank - 1, ::)

        val D = diag(svdA.S)
        val D2 = D(::, 0 to reduced_rank - 1)

        val A2 = U2 * D2 * Vt2


        // We got the reduced rank matrix, let's calculate the average relative errors for each element
        val diff = A - A2
        val rel_diff = diff / A

        import math.abs

        val rel_diff_abs = rel_diff.map(xi => abs(xi))


        calculate_avg_relative_error(original_rank, rel_diff_abs)

        Vt2
    }

    def calculate_avg_relative_error(original_rank: Int, rel_diff_abs: DenseMatrix[Double]) = {
        val sum_rel = sum(rel_diff_abs)
        val avg_rel_error = sum_rel / (original_rank * original_rank)
    }


    // We use this function to examine each topic, and learn what each topic is about

    // def find_most_common_terms_in_topic(topic: Int, num_terms: Int, reduced_v: DenseMatrix[Double]) : Seq[(String, Double)] = {
    //     val term_weights = reduced_v.toArray.zipWithIndex
    //     val sorted = term_weights.sortBy(-_._1)
    //     // select first num_terms elements
    //     sorted.take(num_terms)
    // }


//    We use this function to evaluate our topic modelling algorithm with respect to its classification of docs.
    // def find_most_related_docs_for_topic(topic: Int, num_docs: Int, reduced_u: DenseMatrix[Double]) : Seq[Seq[String, Double]] = {
    //     val doc_weights = reduced_u.toArray.map(_.toArray(topic)).zipWithUniqueId()

    //     doc_weights.top(num_docs)

    // }

}




