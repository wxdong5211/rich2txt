package com.impler.rich2txt.model

import scala.beans.BeanProperty

/**
  * Rule
  */
class OpRule {
  @BeanProperty var op: String = _
  @BeanProperty var prev: String = _
  @BeanProperty var suffix: String = _
  @BeanProperty var args: List[OpArgRule] = _
}
