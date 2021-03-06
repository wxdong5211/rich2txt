package com.impler.rich2txt

import java.text.SimpleDateFormat
import java.util

import com.impler.rich2txt.model.{PageReport, Report, OpReport}
import org.apache.pdfbox.pdmodel.graphics.PDXObject
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject

import scala.collection.JavaConversions._

import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.{COSName, COSDocument, COSBase}
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}

/**
  * PDFTestStripper
  */
class PDFReportStripper extends PDFTextStripper{

  private val buff =  StringBuilder.newBuilder

  private val defaultLineSep = "\n"

  private var minX = Float.MaxValue
  private var maxX = -1f
  private var currY = -1f
//  private var charNumber = 0
//  private var lineNumber = 0
//  private var opNumber = 0

  private var pageMap: Map[Int,PageReport] = Map[Int,PageReport]()

//  private var opMap: Map[String,OpReport] = Map[String,OpReport]()
//  private var lineHeightMap: Map[Float,Int] = Map[Float,Int]()
  private var resourceMap: Map[String,Int] = Map[String,Int]()

  override def processTextPosition(text: TextPosition): Unit = {
    println(s"text => $text @ ${text.getX},${text.getY},${text.getWidthDirAdj},${text.getHeightDir}")
    val pageReport = getCurrPageReport
    var lineHeightMap = pageReport.lineHeightMap
    if(currY != -1 && currY != text.getY){
      buff ++= defaultLineSep
      pageReport.lineNumber += 1
      val lineHeight = text.getY - currY
      if(lineHeightMap.contains(lineHeight)){
        lineHeightMap += lineHeight -> (lineHeightMap(lineHeight)+1)
      }else{
        lineHeightMap += lineHeight -> 1
      }
    }
    currY = text.getY
    if(minX > text.getX){
      minX = text.getX
    }
    if(maxX < text.getX){
      maxX = text.getX
    }
    buff ++= text.getUnicode
    pageReport.charNumber += 1
    pageReport.lineHeightMap = lineHeightMap
    pageReport.minX = minX
    pageReport.maxX = maxX
//    super.processTextPosition(text)
  }

//  override def writeString(text: String, textPositions: util.List[TextPosition]): Unit = {
//    println(s"write => $text , textPositions => ${textPositions.get(0).getFontSize}")
//    super.writeString(text, textPositions)
//  }

  override def processOperator(operator: Operator, operands: util.List[COSBase]): Unit = {
    val name: String = operator.getName
//    if(opMap.contains(name)){
//      opMap(name).times += 1
//      val tag = opMap(name).paramMap.contains(operands)
//      if(tag){
//        opMap(name).paramMap += operands -> (opMap(name).paramMap(operands) + 1)
//      }else{
//        opMap(name).paramMap += operands -> 1
//      }
//    }else{
//      val opReport = new OpReport
//      opReport.times = 1
//      opReport.paramMap = Map(operands -> 1)
//      opMap += name -> opReport
//    }
//    opNumber += 1
    val pageReport = getCurrPageReport
    var opMap = pageReport.opMap
    if(opMap.contains(name)){
      opMap(name).times += 1
      val tag = opMap(name).paramMap.contains(operands)
      if(tag){
        opMap(name).paramMap += operands -> (opMap(name).paramMap(operands) + 1)
      }else{
        opMap(name).paramMap += operands -> 1
      }
    }else{
      val opReport = new OpReport
      opReport.times = 1
      opReport.paramMap = Map(operands -> 1)
      opMap += name -> opReport
    }
    pageReport.opMap = opMap
    pageReport.opNumber += 1

    println(s"Operator $name -> $operands start")
    super.processOperator(operator, operands)
//    rules.foreach(r=>{
//      if(name.equals(r.op)){
//        buff ++= r.suffix
//      }
//    })
    println(s"Operator $name -> $operands end")
  }

  override def getText(doc: PDDocument): String = {
    super.getText(doc)
    buff.toString()
  }

  def getReport(doc: PDDocument): Report ={
    val text = getText(doc)
//    printDocument(doc)
//    println(s"minX = $minX, maxX = $maxX, charNumber = $charNumber, lineNumber = $lineNumber, opNumber = $opNumber")
//    println("OP MAP "+opMap.size)
//    opMap.toSeq.sortBy(_._2.times).foreach(o => println(o._1+" = "+o._2+" "+f"${o._2.times*100/opNumber.toFloat}%.2f"+"%"))
//    var size = lineHeightMap.size
//    println("LineHeight MAP "+size)
//    lineHeightMap.toSeq.sortBy(_._2).foreach(o => println(o._1+" = "+o._2+" "+f"${o._2*100/size.toFloat}%.2f"+"%"))
//    size = resourceMap.size
//    println("ResourceMap MAP "+size)
//    resourceMap.toSeq.sortBy(_._2).foreach(o => println(o._1+" = "+o._2+" "+f"${o._2*100/size.toFloat}%.2f"+"%"))
    println("PageMap MAP "+pageMap.size)
    pageMap.toSeq.sortBy(_._1).foreach(o => printPageReport(o._1,o._2))
    val report = new Report
//    report.text = text
//    report.minX = minX
//    report.maxX = maxX
//    report.charNumber = charNumber
//    report.lineNumber = lineNumber
//    report.opNumber = opNumber
//    report.opMap = opMap
//    report.lineHeightMap = lineHeightMap
//    report.resourceMap = resourceMap
    report
  }

  private def getCurrPageReport: PageReport ={
    val pageNo = getCurrentPageNo
    if(pageMap.contains(pageNo)){
      pageMap(pageNo)
    }else{
      reset()
      val pageReport = new PageReport
      pageMap += pageNo -> pageReport
      pageReport
    }
  }

  private def reset(): Unit ={
    minX = Float.MaxValue
    maxX = -1f
    currY = -1f
  }

  private def printPageReport(pageNo: Int, pageReport: PageReport):Unit={
    println(s"page $pageNo Report = $pageReport")
    println(s"minX = ${pageReport.minX}, maxX = ${pageReport.maxX}, charNumber = ${pageReport.charNumber}, lineNumber = ${pageReport.lineNumber}, opNumber = ${pageReport.opNumber}")
    val opMap = pageReport.opMap
    println("OP MAP "+opMap.size)
    opMap.toSeq.sortBy(_._2.times).foreach(o => println(o._1+" = "+o._2+" "+f"${o._2.times*100/pageReport.opNumber.toFloat}%.2f"+"%"))
    val lineHeightMap = pageReport.lineHeightMap
    var size = lineHeightMap.size
    println("LineHeight MAP "+size)
    lineHeightMap.toSeq.sortBy(_._2).foreach(o => println(o._1+" = "+o._2+" "+f"${o._2*100/size.toFloat}%.2f"+"%"))
    val resourceMap = pageReport.resourceMap
    size = resourceMap.size
    println("ResourceMap MAP "+size)
    resourceMap.toSeq.sortBy(_._2).foreach(o => println(o._1+" = "+o._2+" "+f"${o._2*100/size.toFloat}%.2f"+"%"))
  }

  private def printDocument(document: PDDocument):Unit={
    val pages: Int = document.getNumberOfPages
    println("document = "+document)
    println("document.getEncryption = "+document.getEncryption)
    println("document.getNumberOfPages = "+pages)
    println("document.getVersion = "+document.getVersion)
    println("document.isEncrypted = "+document.isEncrypted)
    printCOSDocument(document.getDocument)
    printDocumentCatalog(document.getDocumentCatalog)
    printDocumentInformation(document.getDocumentInformation)
    for(i <- 0 until pages){
      println("page start "+i)
      printPage(document.getPage(i))
      println("page end "+i)
    }
  }

  private def printPage(page: PDPage):Unit={
    println("page = "+page)
    val resources: PDResources = page.getResources
    val objectNames: Iterable[COSName] = resources.getXObjectNames
    println(objectNames)
    objectNames.zipWithIndex.foreach(x=>{
      val name: String = x._1.toString
      if(resourceMap.contains(name)){
        resourceMap += name -> (resourceMap(name)+1)
      }else{
        resourceMap += name -> 1
      }
      val xObject: PDXObject = resources.getXObject(x._1)
      println("page = "+page)
      xObject match {
        case img: PDImageXObject => printImageXObject(img)//ImageIO.write(img.getImage,img.getSuffix, new File(pdf+"_2_"+x._2+"."+img.getSuffix))
        case _ => println(xObject)
      }
    })
  }

  private def printImageXObject(img: PDImageXObject):Unit={
    println("img = "+img + ",suffix = "+img.getSuffix+",width = "+img.getWidth+",height = "+img.getHeight)
  }

  private def printCOSDocument(document: COSDocument):Unit={
    println("cosDocument = "+document)
    println("cosDocument.getDocumentID = "+document.getDocumentID)
  }

  private def printDocumentInformation(information: PDDocumentInformation):Unit={
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    println("information = "+information)
    println("information.getAuthor = "+information.getAuthor)
    println("information.getCreationDate = "+format.format(information.getCreationDate.getTime))
    println("information.getCreator = "+information.getCreator)
    println("information.getKeywords = "+information.getKeywords)
    println("information.getModificationDate = "+format.format(information.getModificationDate.getTime))
    println("information.getProducer = "+information.getProducer)
    println("information.getSubject = "+information.getSubject)
    println("information.getTitle = "+information.getTitle)
    println("information.getTrapped = "+information.getTrapped)
    println("information.getMetadataKeys = "+information.getMetadataKeys)
    information.getMetadataKeys.foreach(k =>{
      println("metadataKeys "+k+" = "+information.getCustomMetadataValue(k))
    })
  }

  private def printDocumentCatalog(catalog: PDDocumentCatalog):Unit={
    println("documentCatalog = "+catalog)
    printDocumentNameDictionary(catalog.getNames)
  }

  private def printDocumentNameDictionary(dictionary: PDDocumentNameDictionary):Unit={
    println("dictionary = "+dictionary)
    println("dictionary.getCOSObject = "+dictionary.getCOSObject)
    println("dictionary.getDests = "+dictionary.getDests)
    println("dictionary.getEmbeddedFiles = "+dictionary.getEmbeddedFiles)
    println("dictionary.getJavaScript = "+dictionary.getJavaScript)
  }

}
