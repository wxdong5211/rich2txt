package com.impler.rich2txt

import java.util

import com.impler.rich2txt.model.{Report, RuleGroup, OpArgRule, OpRule}
import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.text.{TextPosition, PDFTextStripper}

import scala.collection.JavaConversions._

/**
  * PDFFormatStripper
  */
class PDFFormatStripper extends PDFTextStripper{

  private val buff =  StringBuilder.newBuilder
  private var currRule: OpRule = _

  private val defaultLineSep = "\n"
  private var isLineBegin = true
  private var minX = -1f
  private var maxX = -1f
  private var currY = -1f

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

  //Operator Tf -> [COSName{FXF3}, COSInt{209}] content
  val testRule3 = new OpRule
  testRule3.op = "Tf"
  testRule3.prev = "<DIV>"
  testRule3.suffix = "</DIV>"
  val testRule3Arg1 = new OpArgRule
  testRule3Arg1.arg = "COSInt{209}"
  testRule3Arg1.idx = 1
  testRule3.args = List[OpArgRule](testRule3Arg1)

  //Operator Tf -> [COSName{FXF3}, COSInt{300}] title lv2
  val testRule4 = new OpRule
  testRule4.op = "Tf"
  testRule4.prev = "<H2>"
  testRule4.suffix = "</H2>"
  val testRule4Arg1 = new OpArgRule
  testRule4Arg1.arg = "COSInt{300}"
  testRule4Arg1.idx = 1
  testRule4.args = List[OpArgRule](testRule4Arg1)

  private val rules = List[OpRule](testRuleGroup,testRule3,testRule4)

  override def processTextPosition(text: TextPosition): Unit = {
//    println(s"text => $text @ ${text.getX},${text.getY}")
    if(currY != -1 && currY != text.getY){
      if(text.getX < maxX){
        buff ++= "</P>"
      }//minX = 8.51955, maxX = 449.2795 //TODO need by page scope
      buff ++= defaultLineSep
      isLineBegin = true
    }
    if(isLineBegin){
      if(text.getX > minX){
        buff ++= "<P>"
      }
      isLineBegin = false
    }
    currY = text.getY
    buff ++= text.getUnicode
    //    super.processTextPosition(text)
  }

  //  override def writeString(text: String, textPositions: util.List[TextPosition]): Unit = {
  //    println(s"write => $text , textPositions => ${textPositions.get(0).getFontSize}")
  //    super.writeString(text, textPositions)
  //  }

  override def processOperator(operator: Operator, operands: util.List[COSBase]): Unit = {
    val name: String = operator.getName
//    println(s"Operator $name -> $operands start")

    val outRule = checkOutRule(rules,operator,operands)
    outRule match {
      case Some(r) =>
        if(currRule != r){
          if(currRule != null){
            buff ++= currRule.suffix
          }
          buff ++= r.prev
          currRule = r
        }
      case None =>
    }
    super.processOperator(operator, operands)
    //    rules.foreach(r=>{
    //      if(name.equals(r.op)){
    //        buff ++= r.suffix
    //      }
    //    })
//    println(s"Operator $name -> $operands end")
  }

  override def getText(doc: PDDocument): String = {
    super.getText(doc)
    buff.toString()
  }

  def format(doc: PDDocument, report: Report): String ={
    minX = report.minX
    maxX = report.maxX
    getText(doc)
  }

  private def checkOutRule(rules: List[OpRule], operator: Operator, operands: util.List[COSBase]): Option[OpRule] =
    rules.find(checkRuleOrGroup(_,operator,operands))

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
