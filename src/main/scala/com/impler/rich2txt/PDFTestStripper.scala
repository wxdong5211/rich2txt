package com.impler.rich2txt

import java.util

import org.apache.pdfbox.text.{TextPosition, PDFTextStripper}

/**
  * PDFTestStripper
  */
class PDFTestStripper extends PDFTextStripper{
  override def processTextPosition(text: TextPosition): Unit = super.processTextPosition(text)

  override def writeString(text: String, textPositions: util.List[TextPosition]): Unit = {
    println(s"text => $text , textPositions => ${textPositions.get(0).getFontSize}")
    super.writeString(text, textPositions)
  }
}
