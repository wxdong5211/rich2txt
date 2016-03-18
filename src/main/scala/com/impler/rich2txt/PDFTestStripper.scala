package com.impler.rich2txt

import java.util

import org.apache.pdfbox.contentstream.operator.{OperatorProcessor, Operator}
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.{TextPosition, PDFTextStripper}

/**
  * PDFTestStripper
  */
class PDFTestStripper extends PDFTextStripper{
  private val buff =  StringBuilder.newBuilder
  override def processTextPosition(text: TextPosition): Unit = {
    println(s"text => $text")
    buff ++= text.getUnicode
    super.processTextPosition(text)
  }

  override def writeString(text: String, textPositions: util.List[TextPosition]): Unit = {
    println(s"text => $text , textPositions => ${textPositions.get(0).getFontSize}")
    super.writeString(text, textPositions)
  }

  override def processOperator(operator: Operator, operands: util.List[COSBase]): Unit = {
    val name: String = operator.getName
    println(s"Operator $name start $operands")
    super.processOperator(operator, operands)
    println(s"Operator $name end $operands")
  }

  override def getText(doc: PDDocument): String = {
    val text: String = super.getText(doc)
    println(s"buff = $buff")
    text
  }
}
