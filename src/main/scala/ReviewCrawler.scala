import java.io.FileWriter

import org.jsoup.Jsoup

import scala.io.Source
import scala.util.Try

/**
  * Created by Leonard Hövelmann (leonard.hoevelmann002@stud.fh-dortmund.de on 29.08.2017.
  *
  * Tool to collect review texts from the corpus. Expectes the filepath to "german.corpus.tsv" as the first and the output filepath as the second argument.
  *
  * For some reason, amazon provides two different site structures. These are handled with "firstTry" and "secondTry". However, in cases where amazon changes the site structure or returns
  * errors, this tool might add empty reviews. In this case, you should try it again later.
  *
  */
object ReviewCrawler {

  val reviewUrl = "https://www.amazon.de/gp/customer-reviews/"

  def main(args: Array[String]): Unit = {
    if (args.length < 2) println("You have to provide the filepath for the german.corpus.tsv and the output filepath as the second argument")
    else {
      val filename = args(0)
      val reviewsWithText = Source.fromFile(filename).getLines().toList.tail.map(_.split("\t"))
        .map(review => ReviewWithoutText(review(0), review(1), review(2), review(3).toInt))
        .map(collectReviewText)
      val fw = new FileWriter(args(1))
      fw.write("DOMAIN\tASIN\tID\tRATING\tTITLE\tTEXT\n")
      reviewsWithText.foreach(reviewWithText => {
        fw.write(reviewWithText.domain + "\t")
        fw.write(reviewWithText.asin + "\t")
        fw.write(reviewWithText.id + "\t")
        fw.write(reviewWithText.rating + "\t")
        fw.write(reviewWithText.title + "\t")
        fw.write(reviewWithText.text + "\n")
      })
      fw.close()
      print(f"Written reviews to ${args(1)}\n")
    }

  }

  private def collectReviewText(review: ReviewWithoutText): ReviewWitText = {

    val doc = Jsoup.connect(reviewUrl + review.id)
      .timeout(30000)
      .followRedirects(true)
      .ignoreHttpErrors(true)
      .userAgent("Mozilla/17.0").get

    def getReviewTitle = {
      val firstTry: Try[String] = Try(doc.getElementsByClass("review-title").first.text)
      val secondTry: Try[String] = Try(doc.select(".hReview > .summary").text)
      if (firstTry.isSuccess) firstTry.get
      else if (secondTry.isSuccess) secondTry.get
      else ""
    }

    def getReviewText = {
      val firstTry = Try(doc.getElementsByClass("review-text").first.text)
      val secondTry = Try(doc.select(".hReview > .description").text)
      if (firstTry.isSuccess) firstTry.get
      else if (secondTry.isSuccess) secondTry.get
      else ""
    }


    println(review.id)
    val reviewTitle = getReviewTitle
    println(reviewTitle)
    val reviewText = getReviewText
    println(reviewText)

    ReviewWitText(review.domain, review.asin, review.id, review.rating, reviewTitle, reviewText)
  }

  private sealed case class ReviewWithoutText(domain: String, asin: String, id: String, rating: Int)

  private sealed case class ReviewWitText(domain: String, asin: String, id: String, rating: Int, title: String, text: String)

}
