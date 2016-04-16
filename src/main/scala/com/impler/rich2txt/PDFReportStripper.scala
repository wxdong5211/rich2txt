package com.impler.rich2txt

import java.util

import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.pdmodel.PDDocument
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
  private var charNumber = 0
  private var lineNumber = 0
  private var opNumber = 0

  private var opMap: Map[String,Int] = Map[String,Int]()


  override def processTextPosition(text: TextPosition): Unit = {
    println(s"text => $text @ ${text.getX},${text.getY},${text.getWidthDirAdj},${text.getHeightDir}")
    if(currY != -1 && currY != text.getY){
      buff ++= defaultLineSep
      lineNumber += 1
    }
    currY = text.getY
    if(minX > text.getX){
      minX = text.getX
    }
    if(maxX < text.getX){
      maxX = text.getX
    }
    buff ++= text.getUnicode
    charNumber += 1
//    super.processTextPosition(text)
  }

//  override def writeString(text: String, textPositions: util.List[TextPosition]): Unit = {
//    println(s"write => $text , textPositions => ${textPositions.get(0).getFontSize}")
//    super.writeString(text, textPositions)
//  }

  override def processOperator(operator: Operator, operands: util.List[COSBase]): Unit = {
    val name: String = operator.getName
    if(opMap.contains(name)){
      opMap += name -> (opMap(name)+1)
    }else{
      opMap += name -> 1
    }
    opNumber += 1
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
    val text: String = super.getText(doc)
    println(s"buff = $buff , minX = $minX, maxX = $maxX, charNumber = $charNumber, lineNumber = $lineNumber, opNumber = $opNumber")
    opMap.toSeq.sortBy(_._2).foreach(o => println(o._1+" = "+o._2+" "+f"${o._2*100/opNumber.toFloat}%.2f"+"%"))
    text
  }


}
