package com.impler.rich2txt

import java.util

import com.impler.rich2txt.model.Rule
import org.apache.pdfbox.contentstream.operator.{OperatorProcessor, Operator}
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.{TextPosition, PDFTextStripper}

/**
  * PDFTestStripper
  */
class PDFTestStripper extends PDFTextStripper{
  private val buff =  StringBuilder.newBuilder

  //Operator Tf -> [COSName{FXF1}, COSInt{499}] title
  val testRule = new Rule
  testRule.op = "Tf"
  private val rules = List[Rule](testRule)

  override def processTextPosition(text: TextPosition): Unit = {
    println(s"text => $text")
    buff ++= text.getUnicode
//    super.processTextPosition(text)
  }

//  override def writeString(text: String, textPositions: util.List[TextPosition]): Unit = {
//    println(s"write => $text , textPositions => ${textPositions.get(0).getFontSize}")
//    super.writeString(text, textPositions)
//  }

  override def processOperator(operator: Operator, operands: util.List[COSBase]): Unit = {
    val name: String = operator.getName
    println(s"Operator $name -> $operands start")
    super.processOperator(operator, operands)
    println(s"Operator $name -> $operands end")
  }

  override def getText(doc: PDDocument): String = {
    val text: String = super.getText(doc)
    println(s"buff = $buff")
    text
  }
}
