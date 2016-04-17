package com.impler.rich2txt

import java.io.File

import org.apache.logging.log4j.{LogManager, Logger}
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.text.PDFTextStripper
/**
  * Rich2Txt
  */
object Rich2Txt {

  private final val logger: Logger= LogManager.getLogger(getClass)

  def main(args : Array[String]): Unit = {
    val appHome = System.getenv("APP_HOME")
    logger.info("APP_HOME = {}",appHome)
    val pdf: String = "F:/work/scala/微 第6章.pdf"
    val document = PDDocument.load(new File(pdf))
//    text(document)

    val page: PDPage = document.getPage(1)
    println(page)

    val stripper = new PDFReportStripper()
//    val stripper = new PDFFormatStripper()
    stripper.setStartPage(1)
    stripper.setEndPage(2)
    println(stripper.getText(document))

//    val parser = new PDFStreamParser(page)
//    parser.parse()
//    parser.getTokens.foreach(t=>{
//      println(t.getClass)
//      t match {
//        case obj: COSObject => println("obj = "+obj)
//        case base: COSBase => println("base = "+base)
//        case op: Operator => println("op = "+op)
//        case _ => println("any = "+t)
//      }
//    })

  }

  private def text(document: PDDocument):Unit = {
    val stripper = new PDFTextStripper()
//    stripper.setAddMoreFormatting(true)
//    stripper.setArticleEnd("#####ArticleEnd")
//    stripper.setArticleStart("#####setArticleStart")
//    stripper.setPageEnd("#####PageEnd")
//    stripper.setPageStart("#####PageStart")
//    stripper.setParagraphEnd("#####ParagraphEnd")
//    stripper.setParagraphStart("#####ParagraphStart")
//    stripper.setShouldSeparateByBeads(true)
//    stripper.setSortByPosition(true)
    println(stripper.getText(document))
  }



}
