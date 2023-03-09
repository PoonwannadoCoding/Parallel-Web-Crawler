import org.jsoup.*
import org.jsoup.Connection.Base

import scala.io.Source
import java.net.URL
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.{Failure, Success}
// import knowledge from https://github.com/PoonwannadoCoding/Port-SSC/blob/main/Assignment1/Problem4/src/main/java/Problem4.java
object Crawler {


  var visited: Set[String] = Set()
  var deezSet: Set[String] = Set()
  var extCount: Map[String, Int] = Map()
  var totalWord: Int = 0

  sealed case class WebStats(
                              // the total number of (unique) files found
                              numFiles: Int,
                              // the total number of (unique) file extensions (.jpg is different from .jpeg)
                              numExts: Int,
                              // a map storing the total number of files for each extension.
                              extCounts: Map[String, Int],
                              // the total number of words in all html files combined, excluding
                              // all html tags, attributes and html comments.
                              totalWordCount: Long
                            )

// Crawler
  def gotTaGoFast(link: String, base: URL): Future[Set[String]] = {
    import scala.jdk.CollectionConverters.*
    val fur = Promise[Set[String]]
    try {


      visited += link
      println(link)
      if link.split('/').takeRight(1).mkString.contains('.')  then deezSet += link.substring(link.lastIndexOf("."),link.length) // add extension

      val doc = Jsoup.connect(link).ignoreContentType(true).ignoreHttpErrors(true).get()

      val allLinks = doc.select("a[href]") // html

      val pic = doc.select("img[src]") // image
      val script = doc.select("script[src]") // script
      val imp = doc.select("link[href]") // import
      val picLink = pic.asScala.map(_.attr("abs:src"))
      val scLink = script.asScala.map(_.attr("abs:src"))
      val Link = allLinks.asScala.map(_.attr("abs:href"))
      val impLink = imp.asScala.map(_.attr("abs:href"))
      val savelink = Link ++ picLink ++ impLink ++ scLink
      var frontier: Set[String] = Set()

      // count word
      totalWord = totalWord + allLinks.text().trim.split("\\s+").filter(_.nonEmpty).map(_.replaceAll("[^A-Za-z0-9]", "")).toList.size
      totalWord = totalWord + pic.text().trim.split("\\s+").filter(_.nonEmpty).map(_.replaceAll("[^A-Za-z0-9]", "")).toList.size
      totalWord = totalWord + script.text().trim.split("\\s+").filter(_.nonEmpty).map(_.replaceAll("[^A-Za-z0-9]", "")).toList.size
      totalWord = totalWord + imp.text().trim.split("\\s+").filter(_.nonEmpty).map(_.replaceAll("[^A-Za-z0-9]", "")).toList.size

      savelink.toSet.foreach {
        x => {
          if x.split('/').takeRight(1).mkString.contains('#') then frontier += x.substring(0, x.lastIndexOf("#"))

          else if x.split('/').takeRight(1).mkString.contains('?') then frontier += x.substring(0, x.lastIndexOf("?"))
          else if x.split('/').takeRight(1).mkString.contains('\\') then frontier += x.substring(0, x.lastIndexOf("\\"))

          else if x.charAt(x.length - 1).equals('/') then {
            val newx = x + "index.html"
            frontier += newx
          }
          else frontier += x
        }
      }
      fur.success(frontier)
    } catch {
      case e => e
    }
    fur.future
  }

  def expand(frontier: Set[String], host: URL): Set[String] = {

    frontier.flatMap {
      x => {
        if x.contains(host.getHost) && !visited.contains(x) then Await.result(gotTaGoFast(x, host), Duration.Inf) else Set.empty
      }
    }
  }


  def bfs(src: String, host: URL): Unit = {

    var frontier = Await.result(gotTaGoFast(src, host), Duration.Inf)

    while (frontier.nonEmpty) {
      val frontier_ : Set[String] = expand(frontier, host)

      frontier = frontier_

    }
  }

  def crawlForStats(basePath: String): WebStats = {
    bfs(basePath, URL(basePath))

    deezSet.map{
      extension => extCount += (extension, visited.count(_.contains(extension)))
    }

    WebStats(visited.size, deezSet.size, extCount, totalWord)
  }

  def main(args: Array[String]) = {
    val sampleBasePath: String = "https://cs.muic.mahidol.ac.th/courses/ooc/api/"
    println(crawlForStats(sampleBasePath))
  }

}
