package com.impler.rich2txt.model

import java.util

import org.apache.pdfbox.cos.COSBase

import scala.beans.BeanProperty

/**
  * OpReport
  */
class OpReport {
  @BeanProperty var times: Int = _
  @BeanProperty var paramMap: Map[util.List[COSBase],Int] = _
  override def toString = s"OpReport($times, $paramMap)"
}
