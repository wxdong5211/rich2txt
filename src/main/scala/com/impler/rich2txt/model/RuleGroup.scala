package com.impler.rich2txt.model

import scala.beans.BeanProperty

/**
  * RuleGroup
  */
class RuleGroup extends OpRule{

  @BeanProperty var symbol: String = _
  @BeanProperty var rules: List[OpRule] = _

}
