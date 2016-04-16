package com.impler.rich2txt

import java.awt.image.BufferedImage
import java.io.File
import java.lang.Iterable
import javax.imageio.ImageIO

import org.apache.logging.log4j.{LogManager, Logger}
import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.{COSBase, COSObject, COSName}
import org.apache.pdfbox.pdfparser.{PDFObjectStreamParser, PDFStreamParser}
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.pdmodel.graphics.PDXObject
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}
import org.apache.pdfbox.text.{PDFMarkedContentExtractor, PDFTextStripper}

import scala.collection.JavaConversions._
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
//    printDocument(document)
//    text(document)

//    val dictionary = new PDDocumentNameDictionary(document.getDocumentCatalog)
//    println(dictionary)
//    println(dictionary.getCOSObject)
//    println(dictionary.getDests)
//    println(dictionary.getEmbeddedFiles)
//    println(dictionary.getJavaScript)

    val page: PDPage = document.getPage(1)
    println(page)

    val stripper = new PDFReportStripper()
//    val stripper = new PDFFormatStripper()
    stripper.setStartPage(1)
    stripper.setEndPage(2)
    println(stripper.getText(document))

//    val resources: PDResources = page.getResources
//    val objectNames: Iterable[COSName] = resources.getXObjectNames
//    println(objectNames)
//
//    objectNames.zipWithIndex.foreach(x=>{
//      val xObject: PDXObject = resources.getXObject(x._1)
//      println(xObject)
//      xObject match {
//        case img: PDImageXObject => ImageIO.write(img.getImage,img.getSuffix, new File(pdf+"_2_"+x._2+"."+img.getSuffix))
//        case _ =>
//      }
//    })

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


//    document.getPages.foreach(p=>{
//      println(p)
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

  private def printDocument(document: PDDocument):Unit={
    println("document = "+document)
    println("document.getCurrentAccessPermission = "+document.getCurrentAccessPermission)
    println("document.getDocument = "+document.getDocument)
    println("document.getDocumentCatalog = "+document.getDocumentCatalog)
    println("document.getDocumentId = "+document.getDocumentId)
    println("document.getDocumentInformation = "+document.getDocumentInformation)
    println("document.getEncryption = "+document.getEncryption)
    println("document.getLastSignatureDictionary = "+document.getLastSignatureDictionary)
    println("document.getNumberOfPages = "+document.getNumberOfPages)
    println("document.getPages = "+document.getPages)
    println("document.getResourceCache = "+document.getResourceCache)
    println("document.getSignatureDictionaries = "+document.getSignatureDictionaries)
    println("document.getSignatureFields = "+document.getSignatureFields)
    println("document.getVersion = "+document.getVersion)
    println("document.getClass = "+document.getClass)
    println("document.isAllSecurityToBeRemoved = "+document.isAllSecurityToBeRemoved)
    println("document.isEncrypted = "+document.isEncrypted)
  }

}
