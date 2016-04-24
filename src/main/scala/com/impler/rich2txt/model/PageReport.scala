package com.impler.rich2txt.model

import scala.beans.BeanProperty

/**
  * PageReport
  */
class PageReport {

  @BeanProperty var text: String = _
  @BeanProperty var minX: Float = _
  @BeanProperty var maxX: Float = _
  @BeanProperty var charNumber: Int = _
  @BeanProperty var lineNumber: Int = _
  @BeanProperty var opNumber: Int = _

  @BeanProperty var opMap  = Map[String,OpReport]()
  @BeanProperty var lineHeightMap  = Map[Float,Int]()
  @BeanProperty var resourceMap  = Map[String,Int]()

}
