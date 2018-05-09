object Util {

    /* type aliasing */
    type Topic = String
    type ReviewPost = String
    type Term = String

    def parsePostBody(postBody: ReviewPost) : List[Term] = {

        postBody.split(" +").toList

    }

    def roundToTwoDecimalDigits(num: Double) : Double = {
        BigDecimal(num).setScale(2, BigDecimal.RoundingMode.DOWN).toDouble
    }

}