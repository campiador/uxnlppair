/*
    Util.scala
    Purpose: contains the utility functions
    By: Ge Gao & Behnam Heydarshahi
*/

object Util {

    type Topic = String
    type ReviewPost = String
    type Term = String

    /*
        Purpose: parse the string that contains all the 
                 reviews into a list of reviews
    */
    def parsePostBody(postBody: ReviewPost) : List[Term] = {

        postBody.split(" +").toList

    }

    /*
        Purpose: round a decimal to reserve 2 digits after
                 the decimal point
    */
    def roundToTwoDecimalDigits(num: Double) : Double = {

        BigDecimal(num).setScale(2, BigDecimal.RoundingMode.DOWN).toDouble

    }

    /*
        Purpose: 
    */
    def printType[T](x:T) :Unit = {

        println(x.getClass.toString())

    }

    /*
        Purpose: remove the ith element from array
    */
    def remove(a: Array[Term], i: Int): Array[Term] = {
        val b = a.toBuffer
        b.remove(i)
        b.toArray
    } 

}