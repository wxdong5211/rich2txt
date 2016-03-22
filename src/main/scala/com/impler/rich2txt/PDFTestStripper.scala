package com.impler.rich2txt

import java.util

import com.impler.rich2txt.model.{RuleGroup, OpArgRule, OpRule}
import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.{PDFTextStripper, TextPosition}

import scala.collection.JavaConversions._

/**
  * PDFTestStripper
  */
class PDFTestStripper extends PDFTextStripper{
  private val buff =  StringBuilder.newBuilder

  //Operator Tf -> [COSName{FXF1}, COSInt{499}] title
  val testRule = new OpRule
  testRule.op = "Tf"
  val testRuleArg1 = new OpArgRule
  testRuleArg1.arg = "COSInt{499}"
  testRuleArg1.idx = 1
  testRule.args = List[OpArgRule](testRuleArg1)

  val testRule2 = new OpRule
  testRule2.op = "Tf"
  val testRule2Arg1 = new OpArgRule
  testRule2Arg1.arg = "COSInt{1001}"
  testRule2Arg1.idx = 1
  testRule2.args = List[OpArgRule](testRule2Arg1)

  val testRuleGroup = new RuleGroup
  testRuleGroup.prev = "<H1>"
  testRuleGroup.suffix = "</H1>"
  testRuleGroup.symbol = "or"
  testRuleGroup.rules = List[OpRule](testRule,testRule2)
  private val rules = List[OpRule](testRuleGroup)

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

    val outRules = checkOutRules(rules,operator,operands)
    outRules.foreach(r=>{
      val flag = r match {
        case group: RuleGroup => checkRuleGroup(group,operator,operands)
        case rule: OpRule => checkOpRule(rule,operator,operands)
        case _ => false
      }
      if(flag){
        buff ++= r.prev
      }
    })
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
    println(s"buff = $buff")
    text
  }

  private def checkOutRules(rules: List[OpRule], operator: Operator, operands: util.List[COSBase]): List[OpRule] ={
    var ret = List[OpRule]()
    rules.foreach(r => {
      if(checkRuleOrGroup(r,operator,operands)){
        ret +:= r
      }
    })
    ret
  }

  private def checkRules(rules: List[OpRule], operator: Operator, operands: util.List[COSBase], andThem: Boolean = true): Boolean ={
    var ret = andThem
    rules.foreach(r=>{
      ret = if(andThem){
        ret && checkRuleOrGroup(r,operator,operands)
      }else{
        ret || checkRuleOrGroup(r,operator,operands)
      }
    })
    ret
  }

  private def checkRuleOrGroup(rule: OpRule, operator: Operator, operands: util.List[COSBase]): Boolean = {
    rule match {
      case group: RuleGroup => checkRuleGroup(group,operator,operands)
      case rule: OpRule => checkOpRule(rule,operator,operands)
      case _ => false
    }
  }

  private def checkRuleGroup(group: RuleGroup, operator: Operator, operands: util.List[COSBase]): Boolean = {
    val name: String = operator.getName
    var flag = true
    if(group.op!=null && group.op.length!=0){
      flag = name.equals(group.op)
    }
    if(group.args!=null&&group.args.nonEmpty){
      val length: Int = operands.length
      group.args.foreach(ra=>{
        flag = flag && ra.idx < length && operands(ra.idx).toString.equals(ra.arg)
      })
    }
    flag && checkRules(group.rules, operator, operands, !"or".equals(group.symbol))
  }

  private def checkOpRule(rule: OpRule, operator: Operator, operands: util.List[COSBase]): Boolean = {
    val name: String = operator.getName
    var flag = false
    if(name.equals(rule.op)){
      val length: Int = operands.length
      flag = true
      rule.args.foreach(ra=>{
        flag = flag && ra.idx < length && operands(ra.idx).toString.equals(ra.arg)
      })
    }
    flag
  }
}
