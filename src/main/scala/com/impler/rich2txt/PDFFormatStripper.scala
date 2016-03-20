package com.impler.rich2txt

import java.io._
import java.text.{Normalizer, Bidi}
import java.util
import java.util.regex.Pattern
import java.util.{Collections, StringTokenizer}

import org.apache.fontbox.ttf.TrueTypeFont
import org.apache.fontbox.util.BoundingBox
import org.apache.logging.log4j.LogManager
import org.apache.pdfbox.contentstream.PDFStreamEngine
import org.apache.pdfbox.contentstream.operator.DrawObject
import org.apache.pdfbox.contentstream.operator.state._
import org.apache.pdfbox.contentstream.operator.text._
import org.apache.pdfbox.pdmodel.{PDPageTree, PDDocument, PDPage}
import org.apache.pdfbox.pdmodel.common.PDRectangle
import org.apache.pdfbox.pdmodel.font._
import org.apache.pdfbox.pdmodel.font.encoding.GlyphList
import org.apache.pdfbox.pdmodel.graphics.state.PDGraphicsState
import org.apache.pdfbox.pdmodel.interactive.documentnavigation.outline.PDOutlineItem
import org.apache.pdfbox.text.{TextPositionComparator, TextPosition}
import org.apache.pdfbox.util.{QuickSort, Matrix, Vector}

/**
  * PDFFormatStripper
  */
class PDFFormatStripper extends PDFStreamEngine{

  private val log = LogManager.getLogger(classOf[PDFFormatStripper])
  private var pageRotation: Int = 0
  private var pageSize: PDRectangle = null
  private var translateMatrix: Matrix = null
  private var glyphList: GlyphList = null


  private var defaultIndentThreshold: Float = 2.0f
  private var defaultDropThreshold: Float = 2.5f
  private var useCustomQuickSort: Boolean = false

  var strDrop: String = null
  var strIndent: String = null
  try {
    val className: String = classOf[PDFFormatStripper].getSimpleName.toLowerCase
    var prop: String = className + ".indent"
    strIndent = System.getProperty(prop)
    prop = className + ".drop"
    strDrop = System.getProperty(prop)
  } catch {
    case e: SecurityException =>
  }
  if (strIndent != null && strIndent.length > 0) {
    try {
      defaultIndentThreshold = strIndent.toFloat
    } catch {
      case nfe: NumberFormatException =>
    }
  }
  if (strDrop != null && strDrop.length > 0) {
    try {
      defaultDropThreshold = strDrop.toFloat
    } catch {
      case nfe: NumberFormatException =>
    }
  }

  var is16orLess: Boolean = false
  try {
    val version: String = System.getProperty("java.specification.version")
    val st: StringTokenizer = new StringTokenizer(version, ".")
    val majorVersion: Int = st.nextToken.toInt
    var minorVersion: Int = 0
    if (st.hasMoreTokens) {
      minorVersion = st.nextToken.toInt
    }
    is16orLess = majorVersion == 1 && minorVersion <= 6
  } catch {
    case x: SecurityException =>
    case nfe: NumberFormatException =>
  }
  useCustomQuickSort = !is16orLess

  protected val LINE_SEPARATOR = System.getProperty("line.separator")

  private var lineSeparator: String = LINE_SEPARATOR
  private var wordSeparator: String = " "
  private var paragraphStart: String = ""
  private var paragraphEnd: String = ""
  private var pageStart: String = ""
  private var pageEnd: String = LINE_SEPARATOR
  private var articleStart: String = ""
  private var articleEnd: String = ""

  private var currentPageNo: Int = 0
  private var startPage: Int = 1
  private var endPage: Int = Int.MaxValue
  private var startBookmark: PDOutlineItem = null

  private var startBookmarkPageNumber = -1
  private var endBookmarkPageNumber = -1

  private var endBookmark: PDOutlineItem = null
  private var suppressDuplicateOverlappingText: Boolean = true
  private var shouldSeparateByBeads: Boolean = true
  private var sortByPosition: Boolean = false
  private var addMoreFormatting: Boolean = false

  private var indentThreshold: Float = defaultIndentThreshold
  private var dropThreshold: Float = defaultDropThreshold

  private var spacingTolerance = .5f
  private var averageCharTolerance = .3f

  private var beadRectangles: util.List[PDRectangle] = null

  protected var charactersByArticle = new util.ArrayList[util.List[TextPosition]]

  private val characterListMapping = new util.HashMap[ String,util.TreeMap[Float, util.TreeSet [ Float ]]]

  protected var document: PDDocument = null
  protected var output: Writer = null

  private var inParagraph: Boolean = _

  addOperator(new BeginText)
  addOperator(new Concatenate)
  addOperator(new DrawObject)
  addOperator(new EndText)
  addOperator(new SetGraphicsStateParameters)
  addOperator(new Save)
  addOperator(new Restore)
  addOperator(new NextLine)
  addOperator(new SetCharSpacing)
  addOperator(new MoveText)
  addOperator(new MoveTextSetLeading)
  addOperator(new SetFontAndSize)
  addOperator(new ShowText)
  addOperator(new ShowTextAdjusted)
  addOperator(new SetTextLeading)
  addOperator(new SetMatrix)
  addOperator(new SetTextRenderingMode)
  addOperator(new SetTextRise)
  addOperator(new SetWordSpacing)
  addOperator(new SetTextHorizontalScaling)
  addOperator(new ShowTextLine)
  addOperator(new ShowTextLineAndSpace)
  val path: String = "org/apache/pdfbox/resources/glyphlist/additional.txt"
  val input: InputStream = classOf[GlyphList].getClassLoader.getResourceAsStream(path)
  glyphList = new GlyphList(GlyphList.getAdobeGlyphList, input)

  @throws(classOf[IOException])
  def processPageText(page: PDPage) {
    this.pageRotation = page.getRotation
    this.pageSize = page.getCropBox
    if (pageSize.getLowerLeftX == 0 && pageSize.getLowerLeftY == 0) {
      translateMatrix = null
    }
    else {
      translateMatrix = Matrix.getTranslateInstance(-pageSize.getLowerLeftX, -pageSize.getLowerLeftY)
    }
    super.processPage(page)
  }

  @throws(classOf[IOException])
  protected override def showGlyph(textRenderingMatrix: Matrix, font: PDFont, code: Int, unicode: String, displacement: Vector) {
    val state: PDGraphicsState = getGraphicsState
    val ctm: Matrix = state.getCurrentTransformationMatrix
    val fontSize: Float = state.getTextState.getFontSize
    val horizontalScaling: Float = state.getTextState.getHorizontalScaling / 100f
    val textMatrix: Matrix = getTextMatrix
    val bbox: BoundingBox = font.getBoundingBox
    if (bbox.getLowerLeftY < Short.MinValue) {
      bbox.setLowerLeftY(-(bbox.getLowerLeftY + 65536))
    }
    var glyphHeight: Float = bbox.getHeight / 2
    val fontDescriptor: PDFontDescriptor = font.getFontDescriptor
    if (fontDescriptor != null) {
      val capHeight: Float = fontDescriptor.getCapHeight
      if (capHeight != 0 && capHeight < glyphHeight) {
        glyphHeight = capHeight
      }
    }
    var height: Float = .0f
    if (font.isInstanceOf[PDType3Font]) {
      height = font.getFontMatrix.transformPoint(0, glyphHeight).y
    }
    else {
      height = glyphHeight / 1000
    }
    var displacementX: Float = displacement.getX
    if (font.isVertical) {
      displacementX = font.getWidth(code) / 1000
      var ttf: TrueTypeFont = null
      font match {
        case ptf: PDTrueTypeFont => ttf = ptf.getTrueTypeFont
        case pof: PDType0Font =>
          pof.getDescendantFont match {
            case fontType: PDCIDFontType2 => ttf = fontType.getTrueTypeFont
            case _ =>
          }
        case _ =>
      }
      if (ttf != null && ttf.getUnitsPerEm != 1000) {
        displacementX *= 1000f / ttf.getUnitsPerEm
      }
    }
    val tx: Float = displacementX * fontSize * horizontalScaling
    val ty: Float = displacement.getY * fontSize
    val td: Matrix = Matrix.getTranslateInstance(tx, ty)
    val nextTextRenderingMatrix: Matrix = td.multiply(textMatrix).multiply(ctm)
    var nextX: Float = nextTextRenderingMatrix.getTranslateX
    var nextY: Float = nextTextRenderingMatrix.getTranslateY
    val dxDisplay: Float = nextX - textRenderingMatrix.getTranslateX
    val dyDisplay: Float = height * textRenderingMatrix.getScalingFactorY
    var glyphSpaceToTextSpaceFactor: Float = 1 / 1000f
    if (font.isInstanceOf[PDType3Font]) {
      glyphSpaceToTextSpaceFactor = font.getFontMatrix.getScaleX
    }
    var spaceWidthText: Float = 0
    try {
      spaceWidthText = font.getSpaceWidth * glyphSpaceToTextSpaceFactor
    } catch {
      case exception: Throwable => log.warn(exception, exception)
    }
    if (spaceWidthText == 0) {
      spaceWidthText = font.getAverageFontWidth * glyphSpaceToTextSpaceFactor
      spaceWidthText *= .80f
    }
    if (spaceWidthText == 0) {
      spaceWidthText = 1.0f
    }
    val spaceWidthDisplay: Float = spaceWidthText * textRenderingMatrix.getScalingFactorX
    var unicode = font.toUnicode(code, glyphList)
    if (unicode == null) {
      if (font.isInstanceOf[PDSimpleFont]) {
        val c: Char = code.toChar
        unicode = new String(Array[Char](c))
      }
      else {
        return
      }
    }
    var translatedTextRenderingMatrix: Matrix = null
    if (translateMatrix == null) {
      translatedTextRenderingMatrix = textRenderingMatrix
    }
    else {
      translatedTextRenderingMatrix = Matrix.concatenate(translateMatrix, textRenderingMatrix)
      nextX -= pageSize.getLowerLeftX
      nextY -= pageSize.getLowerLeftY
    }
    processTextPosition(new TextPosition(pageRotation, pageSize.getWidth, pageSize.getHeight, translatedTextRenderingMatrix, nextX, nextY, dyDisplay, dxDisplay, spaceWidthDisplay, unicode, Array[Int](code), font, fontSize, (fontSize * textRenderingMatrix.getScalingFactorX).toInt))
  }

  /**
    * This will return the text of a document. See writeText. <br />
    * NOTE: The document must not be encrypted when coming into this method.
    *
    * @param doc The document to get the text from.
    * @return The text of the PDF document.
    * @throws IOException if the doc state is invalid or it is encrypted.
    */
  @throws(classOf[IOException])
  def getText(doc: PDDocument): String = {
    val outputStream: StringWriter = new StringWriter
    writeText(doc, outputStream)
    outputStream.toString
  }

  private def resetEngine() {
    currentPageNo = 0
    document = null
    if (charactersByArticle != null) {
      charactersByArticle.clear()
    }
    if (characterListMapping != null) {
      characterListMapping.clear()
    }
  }

  /**
    * This will take a PDDocument and write the text of that document to the print writer.
    *
    * @param doc          The document to get the data from.
    * @param outputStream The location to put the text.
    * @throws IOException If the doc is in an invalid state.
    */
  @throws(classOf[IOException])
  def writeText(doc: PDDocument, outputStream: Writer) {
    resetEngine()
    document = doc
    output = outputStream
    if (getAddMoreFormatting) {
      paragraphEnd = lineSeparator
      pageStart = lineSeparator
      articleStart = lineSeparator
      articleEnd = lineSeparator
    }
    startDocument(document)
    processPages(document.getPages)
    endDocument(document)
  }

  /**
    * This will process all of the pages and the text that is in them.
    *
    * @param pages The pages object in the document.
    * @throws IOException If there is an error parsing the text.
    */
  @throws(classOf[IOException])
  protected def processPages(pages: PDPageTree) {
    val startBookmarkPage: PDPage = if (startBookmark == null) null else startBookmark.findDestinationPage(document)
    if (startBookmarkPage != null) {
      startBookmarkPageNumber = pages.indexOf(startBookmarkPage) + 1
    }
    else {
      startBookmarkPageNumber = -1
    }
    val endBookmarkPage: PDPage = if (endBookmark == null) null else endBookmark.findDestinationPage(document)
    if (endBookmarkPage != null) {
      endBookmarkPageNumber = pages.indexOf(endBookmarkPage) + 1
    }
    else {
      endBookmarkPageNumber = -1
    }
    if (startBookmarkPageNumber == -1 && startBookmark != null && endBookmarkPageNumber == -1 && endBookmark != null
      && startBookmark.getCOSObject == endBookmark.getCOSObject) {
      startBookmarkPageNumber = 0
      endBookmarkPageNumber = 0
    }
    import scala.collection.JavaConversions._
    for (page <- pages) {
      currentPageNo += 1
      if (page.hasContents) {
        processPage(page)
      }
    }
  }

  /**
    * This method is available for subclasses of this class. It will be called before processing of the document start.
    *
    * @param document The PDF document that is being processed.
    * @throws IOException If an IO error occurs.
    */
  @throws(classOf[IOException])
  protected def startDocument(document: PDDocument) {
  }

  /**
    * This method is available for subclasses of this class. It will be called after processing of the document
    * finishes.
    *
    * @param document The PDF document that is being processed.
    * @throws IOException If an IO error occurs.
    */
  @throws(classOf[IOException])
  protected def endDocument(document: PDDocument) {
  }

  /**
    * This will process the contents of a page.
    *
    * @param page The page to process.
    * @throws IOException If there is an error processing the page.
    */
  @throws(classOf[IOException])
  override def processPage(page: PDPage) {
    if (currentPageNo >= startPage && currentPageNo <= endPage && (startBookmarkPageNumber == -1 || currentPageNo >= startBookmarkPageNumber) && (endBookmarkPageNumber == -1 || currentPageNo <= endBookmarkPageNumber)) {
      startPage(page)
      var numberOfArticleSections: Int = 1
      if (shouldSeparateByBeads) {
        fillBeadRectangles(page)
        numberOfArticleSections += beadRectangles.size * 2
      }
      val originalSize: Int = charactersByArticle.size
      charactersByArticle.ensureCapacity(numberOfArticleSections)
      val lastIndex: Int = Math.max(numberOfArticleSections, originalSize)
      for (i <- 0 until lastIndex)  {
        if (i < originalSize) {
          charactersByArticle.get(i).clear()
        } else {
          if (numberOfArticleSections < originalSize) {
            charactersByArticle.remove(i)
          } else {
            charactersByArticle.add(new util.ArrayList[TextPosition])
          }
        }
      }
      characterListMapping.clear()
      processPageText(page)
      writePage()
      endPage(page)
    }
  }

  private def fillBeadRectangles(page: PDPage) {
    beadRectangles = new util.ArrayList[PDRectangle]
    import scala.collection.JavaConversions._
    for (bead <- page.getThreadBeads) {
      if (bead == null) {
        beadRectangles.add(null)
      } else {
        val rect: PDRectangle = bead.getRectangle
        val mediaBox: PDRectangle = page.getMediaBox
        val upperRightY: Float = mediaBox.getUpperRightY - rect.getLowerLeftY
        val lowerLeftY: Float = mediaBox.getUpperRightY - rect.getUpperRightY
        rect.setLowerLeftY(lowerLeftY)
        rect.setUpperRightY(upperRightY)
        val cropBox: PDRectangle = page.getCropBox
        if (cropBox.getLowerLeftX != 0 || cropBox.getLowerLeftY != 0) {
          rect.setLowerLeftX(rect.getLowerLeftX - cropBox.getLowerLeftX)
          rect.setLowerLeftY(rect.getLowerLeftY - cropBox.getLowerLeftY)
          rect.setUpperRightX(rect.getUpperRightX - cropBox.getLowerLeftX)
          rect.setUpperRightY(rect.getUpperRightY - cropBox.getLowerLeftY)
        }
        beadRectangles.add(rect)
      }
    }
  }

  /**
    * Start a new article, which is typically defined as a column on a single page (also referred to as a bead). This
    * assumes that the primary direction of text is left to right. Default implementation is to do nothing. Subclasses
    * may provide additional information.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  @throws(classOf[IOException])
  protected def startArticle() {
    startArticle(true)
  }

  /**
    * Start a new article, which is typically defined as a column on a single page (also referred to as a bead).
    * Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param isLTR true if primary direction of text is left to right.
    * @throws IOException If there is any error writing to the stream.
    */
  @throws(classOf[IOException])
  protected def startArticle(isLTR: Boolean) {
    output.write(getArticleStart)
  }

  /**
    * End an article. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @throws IOException If there is any error writing to the stream.
    */
  @throws(classOf[IOException])
  protected def endArticle() {
    output.write(getArticleEnd)
  }

  /**
    * Start a new page. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param page The page we are about to process.
    * @throws IOException If there is any error writing to the stream.
    */
  @throws(classOf[IOException])
  protected def startPage(page: PDPage) {
  }

  /**
    * End a page. Default implementation is to do nothing. Subclasses may provide additional information.
    *
    * @param page The page we are about to process.
    * @throws IOException If there is any error writing to the stream.
    */
  @throws(classOf[IOException])
  protected def endPage(page: PDPage) {
  }

  private val END_OF_LAST_TEXT_X_RESET_VALUE: Float = -1
  private val MAX_Y_FOR_LINE_RESET_VALUE: Float = -Float.MaxValue
  private val EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE: Float = -Float.MaxValue
  private val MAX_HEIGHT_FOR_LINE_RESET_VALUE: Float = -1
  private val MIN_Y_TOP_FOR_LINE_RESET_VALUE: Float = Float.MaxValue
  private val LAST_WORD_SPACING_RESET_VALUE: Float = -1

  /**
    * This will print the text of the processed page to "output". It will estimate, based on the coordinates of the
    * text, where newlines and word spacings should be placed. The text will be sorted only if that feature was
    * enabled.
    *
    * @throws IOException If there is an error writing the text.
    */
  @throws(classOf[IOException])
  protected def writePage() {
    var maxYForLine: Float = MAX_Y_FOR_LINE_RESET_VALUE
    var minYTopForLine: Float = MIN_Y_TOP_FOR_LINE_RESET_VALUE
    var endOfLastTextX: Float = END_OF_LAST_TEXT_X_RESET_VALUE
    var lastWordSpacing: Float = LAST_WORD_SPACING_RESET_VALUE
    var maxHeightForLine: Float = MAX_HEIGHT_FOR_LINE_RESET_VALUE
    var lastPosition: PositionWrapper = null
    var lastLineStartPosition: PositionWrapper = null
    var startOfPage: Boolean = true
    var startOfArticle: Boolean = false
    if (charactersByArticle.size > 0) {
      writePageStart()
    }
    import scala.collection.JavaConversions._
    for (textList <- charactersByArticle) {
      if (getSortByPosition) {
        val comparator: TextPositionComparator = new TextPositionComparator
        if (useCustomQuickSort) {
          QuickSort.sort(textList, comparator)
        }
        else {
          Collections.sort(textList, comparator)
        }
      }
      var textIter: util.Iterator[TextPosition] = textList.iterator
      startArticle()
      startOfArticle = true
      val line: util.List[LineItem] = new util.ArrayList[LineItem]
      textIter = textList.iterator
      var previousAveCharWidth: Float = -1
      while (textIter.hasNext) {
        val position: TextPosition = textIter.next
        val current: PositionWrapper = new PositionWrapper(position)
        val characterValue: String = position.getUnicode
        if (lastPosition != null && (position.getFont != lastPosition.getTextPosition.getFont || position.getFontSize != lastPosition.getTextPosition.getFontSize)) {
          previousAveCharWidth = -1
        }
        var positionX: Float = .0f
        var positionY: Float = .0f
        var positionWidth: Float = .0f
        var positionHeight: Float = .0f
        if (getSortByPosition) {
          positionX = position.getXDirAdj
          positionY = position.getYDirAdj
          positionWidth = position.getWidthDirAdj
          positionHeight = position.getHeightDir
        } else {
          positionX = position.getX
          positionY = position.getY
          positionWidth = position.getWidth
          positionHeight = position.getHeight
        }
        val wordCharCount: Int = position.getIndividualWidths.length
        val wordSpacing: Float = position.getWidthOfSpace
        var deltaSpace: Float = .0f
        if (wordSpacing == 0 || java.lang.Float.isNaN(wordSpacing)) {
          deltaSpace = Float.MaxValue
        } else {
          if (lastWordSpacing < 0) {
            deltaSpace = wordSpacing * getSpacingTolerance
          } else {
            deltaSpace = (wordSpacing + lastWordSpacing) / 2f * getSpacingTolerance
          }
        }
        var averageCharWidth: Float = .0f
        if (previousAveCharWidth < 0) {
          averageCharWidth = positionWidth / wordCharCount
        } else {
          averageCharWidth = (previousAveCharWidth + positionWidth / wordCharCount) / 2f
        }
        val deltaCharWidth: Float = averageCharWidth * getAverageCharTolerance
        var expectedStartOfNextWordX: Float = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
        if (endOfLastTextX != END_OF_LAST_TEXT_X_RESET_VALUE) {
          if (deltaCharWidth > deltaSpace) {
            expectedStartOfNextWordX = endOfLastTextX + deltaSpace
          } else {
            expectedStartOfNextWordX = endOfLastTextX + deltaCharWidth
          }
        }
        if (lastPosition != null) {
          if (startOfArticle) {
            lastPosition.setArticleStart()
            startOfArticle = false
          }
          if (!overlap(positionY, positionHeight, maxYForLine, maxHeightForLine)) {
            writeLine(normalize(line))
            line.clear()
            lastLineStartPosition = handleLineSeparation(current, lastPosition, lastLineStartPosition, maxHeightForLine)
            expectedStartOfNextWordX = EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE
            maxYForLine = MAX_Y_FOR_LINE_RESET_VALUE
            maxHeightForLine = MAX_HEIGHT_FOR_LINE_RESET_VALUE
            minYTopForLine = MIN_Y_TOP_FOR_LINE_RESET_VALUE
          }
          if (expectedStartOfNextWordX != EXPECTED_START_OF_NEXT_WORD_X_RESET_VALUE && expectedStartOfNextWordX < positionX && lastPosition.getTextPosition.getUnicode != null && !lastPosition.getTextPosition.getUnicode.endsWith(" ")) {
            line.add(LineItem.getWordSeparator)
          }
        }
        if (positionY >= maxYForLine) {
          maxYForLine = positionY
        }
        endOfLastTextX = positionX + positionWidth
        if (characterValue != null) {
          if (startOfPage && lastPosition == null) {
            writeParagraphStart()
          }
          line.add(new LineItem(position))
        }
        maxHeightForLine = Math.max(maxHeightForLine, positionHeight)
        minYTopForLine = Math.min(minYTopForLine, positionY - positionHeight)
        lastPosition = current
        if (startOfPage) {
          lastPosition.setParagraphStart()
          lastPosition.setLineStart()
          lastLineStartPosition = lastPosition
          startOfPage = false
        }
        lastWordSpacing = wordSpacing
        previousAveCharWidth = averageCharWidth
      }
      if (line.size > 0) {
        writeLine(normalize(line))
        writeParagraphEnd()
      }
      endArticle()
    }
    writePageEnd()
  }

  private def overlap(y1: Float, height1: Float, y2: Float, height2: Float): Boolean = {
    within(y1, y2, .1f) || y2 <= y1 && y2 >= y1 - height1 || y1 <= y2 && y1 >= y2 - height2
  }

  /**
    * Write the line separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the lineseparator to the document.
    */
  @throws(classOf[IOException])
  protected def writeLineSeparator() {
    output.write(getLineSeparator)
  }

  /**
    * Write the word separator value to the output stream.
    *
    * @throws IOException If there is a problem writing out the wordseparator to the document.
    */
  @throws(classOf[IOException])
  protected def writeWordSeparator() {
    output.write(getWordSeparator)
  }

  /**
    * Write the string in TextPosition to the output stream.
    *
    * @param text The text to write to the stream.
    * @throws IOException If there is an error when writing the text.
    */
  @throws(classOf[IOException])
  protected def writeCharacters(text: TextPosition) {
    output.write(text.getUnicode)
  }

  /**
    * Write a Java string to the output stream. The default implementation will ignore the <code>textPositions</code>
    * and just calls `writeString(String)`.
    *
    * @param text          The text to write to the stream.
    * @param textPositions The TextPositions belonging to the text.
    * @throws IOException If there is an error when writing the text.
    */
  @throws(classOf[IOException])
  protected def writeString(text: String, textPositions: util.List[TextPosition]) {
    writeString(text)
  }

  /**
    * Write a Java string to the output stream.
    *
    * @param text The text to write to the stream.
    * @throws IOException If there is an error when writing the text.
    */
  @throws(classOf[IOException])
  protected def writeString(text: String) {
    output.write(text)
  }

  /**
    * This will determine of two floating point numbers are within a specified variance.
    *
    * @param first    The first number to compare to.
    * @param second   The second number to compare to.
    * @param variance The allowed variance.
    */
  private def within(first: Float, second: Float, variance: Float): Boolean = {
    second < first + variance && second > first - variance
  }

  /**
    * This will process a TextPosition object and add the text to the list of characters on a page. It takes care of
    * overlapping text.
    *
    * @param text The text to process.
    */
  protected def processTextPosition(text: TextPosition) {
    var showCharacter: Boolean = true
    if (suppressDuplicateOverlappingText) {
      showCharacter = false
      val textCharacter: String = text.getUnicode
      val textX: Float = text.getX
      val textY: Float = text.getY
      var sameTextCharacters: util.TreeMap[Float, util.TreeSet[Float]] = characterListMapping.get(textCharacter)
      if (sameTextCharacters == null) {
        sameTextCharacters = new util.TreeMap[Float, util.TreeSet[Float]]
        characterListMapping.put(textCharacter, sameTextCharacters)
      }
      var suppressCharacter: Boolean = false
      val tolerance: Float = text.getWidth / textCharacter.length / 3.0f
      val xMatches: util.SortedMap[Float, util.TreeSet[Float]] = sameTextCharacters.subMap(textX - tolerance, textX + tolerance)
      import scala.collection.JavaConversions._
      for (xMatch <- xMatches.values) {
        if(!suppressCharacter){
          val yMatches: util.SortedSet[Float] = xMatch.subSet(textY - tolerance, textY + tolerance)
          if (yMatches.nonEmpty) {
            suppressCharacter = true
          }
        }
      }
      if (!suppressCharacter) {
        var ySet: util.TreeSet[Float] = sameTextCharacters.get(textX)
        if (ySet == null) {
          ySet = new util.TreeSet[Float]
          sameTextCharacters.put(textX, ySet)
        }
        ySet.add(textY)
        showCharacter = true
      }
    }
    if (showCharacter) {
      var foundArticleDivisionIndex: Int = -1
      var notFoundButFirstLeftAndAboveArticleDivisionIndex: Int = -1
      var notFoundButFirstLeftArticleDivisionIndex: Int = -1
      var notFoundButFirstAboveArticleDivisionIndex: Int = -1
      val x: Float = text.getX
      val y: Float = text.getY
      if (shouldSeparateByBeads) {
        for (i <- 0 until beadRectangles.size ; if foundArticleDivisionIndex == -1) {
          val rect: PDRectangle = beadRectangles.get(i)
          if (rect != null) {
            if (rect.contains(x, y)) {
              foundArticleDivisionIndex = i * 2 + 1
            } else if ((x < rect.getLowerLeftX || y < rect.getUpperRightY) && notFoundButFirstLeftAndAboveArticleDivisionIndex == -1) {
              notFoundButFirstLeftAndAboveArticleDivisionIndex = i * 2
            } else if (x < rect.getLowerLeftX && notFoundButFirstLeftArticleDivisionIndex == -1) {
              notFoundButFirstLeftArticleDivisionIndex = i * 2
            } else if (y < rect.getUpperRightY && notFoundButFirstAboveArticleDivisionIndex == -1) {
              notFoundButFirstAboveArticleDivisionIndex = i * 2
            }
          } else {
            foundArticleDivisionIndex = 0
          }
        }
      } else {
        foundArticleDivisionIndex = 0
      }
      var articleDivisionIndex: Int = 0
      if (foundArticleDivisionIndex != -1) {
        articleDivisionIndex = foundArticleDivisionIndex
      } else if (notFoundButFirstLeftAndAboveArticleDivisionIndex != -1) {
        articleDivisionIndex = notFoundButFirstLeftAndAboveArticleDivisionIndex
      } else if (notFoundButFirstLeftArticleDivisionIndex != -1) {
        articleDivisionIndex = notFoundButFirstLeftArticleDivisionIndex
      } else if (notFoundButFirstAboveArticleDivisionIndex != -1) {
        articleDivisionIndex = notFoundButFirstAboveArticleDivisionIndex
      } else {
        articleDivisionIndex = charactersByArticle.size - 1
      }
      val textList: util.List[TextPosition] = charactersByArticle.get(articleDivisionIndex)
      if (textList.isEmpty) {
        textList.add(text)
      } else {
        val previousTextPosition: TextPosition = textList.get(textList.size - 1)
        if (text.isDiacritic && previousTextPosition.contains(text)) {
          previousTextPosition.mergeDiacritic(text)
        } else if (previousTextPosition.isDiacritic && text.contains(previousTextPosition)) {
          text.mergeDiacritic(previousTextPosition)
          textList.remove(textList.size - 1)
          textList.add(text)
        } else {
          textList.add(text)
        }
      }
    }
  }

  /**
    * This is the page that the text extraction will start on. The pages start at page 1. For example in a 5 page PDF
    * document, if the start page is 1 then all pages will be extracted. If the start page is 4 then pages 4 and 5 will
    * be extracted. The default value is 1.
    *
    * @return Value of property startPage.
    */
  def getStartPage: Int = {
    startPage
  }

  /**
    * This will set the first page to be extracted by this class.
    *
    * @param startPageValue New value of 1-based startPage property.
    */
  def setStartPage(startPageValue: Int) {
    startPage = startPageValue
  }

  /**
    * This will get the last page that will be extracted. This is inclusive, for example if a 5 page PDF an endPage
    * value of 5 would extract the entire document, an end page of 2 would extract pages 1 and 2. This defaults to
    * Integer.MAX_VALUE such that all pages of the pdf will be extracted.
    *
    * @return Value of property endPage.
    */
  def getEndPage: Int = {
    endPage
  }

  /**
    * This will set the last page to be extracted by this class.
    *
    * @param endPageValue New value of 1-based endPage property.
    */
  def setEndPage(endPageValue: Int) {
    endPage = endPageValue
  }

  /**
    * Set the desired line separator for output text. The line.separator system property is used if the line separator
    * preference is not set explicitly using this method.
    *
    * @param separator The desired line separator string.
    */
  def setLineSeparator(separator: String) {
    lineSeparator = separator
  }

  /**
    * This will get the line separator.
    *
    * @return The desired line separator string.
    */
  def getLineSeparator: String = {
    lineSeparator
  }

  /**
    * This will get the word separator.
    *
    * @return The desired word separator string.
    */
  def getWordSeparator: String = {
    wordSeparator
  }

  /**
    * Set the desired word separator for output text. The PDFBox text extraction algorithm will output a space
    * character if there is enough space between two words. By default a space character is used. If you need and
    * accurate count of characters that are found in a PDF document then you might want to set the word separator to
    * the empty string.
    *
    * @param separator The desired page separator string.
    */
  def setWordSeparator(separator: String) {
    wordSeparator = separator
  }

  /**
    * @return Returns the suppressDuplicateOverlappingText.
    */
  def getSuppressDuplicateOverlappingText: Boolean = {
    suppressDuplicateOverlappingText
  }

  /**
    * Get the current page number that is being processed.
    *
    * @return A 1 based number representing the current page.
    */
  protected def getCurrentPageNo: Int = {
    currentPageNo
  }

  /**
    * The output stream that is being written to.
    *
    * @return The stream that output is being written to.
    */
  protected def getOutput: Writer = {
    output
  }

  /**
    * Character strings are grouped by articles. It is quite common that there will only be a single article. This
    * returns a List that contains List objects, the inner lists will contain TextPosition objects.
    *
    * @return A double List of TextPositions for all text strings on the page.
    */
  protected def getCharactersByArticle: util.List[util.List[TextPosition]] = {
    charactersByArticle
  }

  /**
    * By default the text stripper will attempt to remove text that overlapps each other. Word paints the same
    * character several times in order to make it look bold. By setting this to false all text will be extracted, which
    * means that certain sections will be duplicated, but better performance will be noticed.
    *
    * @param suppressDuplicateOverlappingTextValue The suppressDuplicateOverlappingText to set.
    */
  def setSuppressDuplicateOverlappingText(suppressDuplicateOverlappingTextValue: Boolean) {
    suppressDuplicateOverlappingText = suppressDuplicateOverlappingTextValue
  }

  /**
    * This will tell if the text stripper should separate by beads.
    *
    * @return If the text will be grouped by beads.
    */
  def getSeparateByBeads: Boolean = {
    shouldSeparateByBeads
  }

  /**
    * Set if the text stripper should group the text output by a list of beads. The default value is true!
    *
    * @param aShouldSeparateByBeads The new grouping of beads.
    */
  def setShouldSeparateByBeads(aShouldSeparateByBeads: Boolean) {
    shouldSeparateByBeads = aShouldSeparateByBeads
  }

  /**
    * Get the bookmark where text extraction should end, inclusive. Default is null.
    *
    * @return The ending bookmark.
    */
  def getEndBookmark: PDOutlineItem = {
    endBookmark
  }

  /**
    * Set the bookmark where the text extraction should stop.
    *
    * @param aEndBookmark The ending bookmark.
    */
  def setEndBookmark(aEndBookmark: PDOutlineItem) {
    endBookmark = aEndBookmark
  }

  /**
    * Get the bookmark where text extraction should start, inclusive. Default is null.
    *
    * @return The starting bookmark.
    */
  def getStartBookmark: PDOutlineItem = {
    startBookmark
  }

  /**
    * Set the bookmark where text extraction should start, inclusive.
    *
    * @param aStartBookmark The starting bookmark.
    */
  def setStartBookmark(aStartBookmark: PDOutlineItem) {
    startBookmark = aStartBookmark
  }

  /**
    * This will tell if the text stripper should add some more text formatting.
    *
    * @return true if some more text formatting will be added
    */
  def getAddMoreFormatting: Boolean = {
    addMoreFormatting
  }

  /**
    * There will some additional text formatting be added if addMoreFormatting is set to true. Default is false.
    *
    * @param newAddMoreFormatting Tell PDFBox to add some more text formatting
    */
  def setAddMoreFormatting(newAddMoreFormatting: Boolean) {
    addMoreFormatting = newAddMoreFormatting
  }

  /**
    * This will tell if the text stripper should sort the text tokens before writing to the stream.
    *
    * @return true If the text tokens will be sorted before being written.
    */
  def getSortByPosition: Boolean = {
    sortByPosition
  }

  /**
    * The order of the text tokens in a PDF file may not be in the same as they appear visually on the screen. For
    * example, a PDF writer may write out all text by font, so all bold or larger text, then make a second pass and
    * write out the normal text.<br/>
    * The default is to <b>not</b> sort by position.<br/>
    * <br/>
    * A PDF writer could choose to write each character in a different order. By default PDFBox does <b>not</b> sort
    * the text tokens before processing them due to performance reasons.
    *
    * @param newSortByPosition Tell PDFBox to sort the text positions.
    */
  def setSortByPosition(newSortByPosition: Boolean) {
    sortByPosition = newSortByPosition
  }

  /**
    * Get the current space width-based tolerance value that is being used to estimate where spaces in text should be
    * added. Note that the default value for this has been determined from trial and error.
    *
    * @return The current tolerance / scaling factor
    */
  def getSpacingTolerance: Float = {
    spacingTolerance
  }

  /**
    * Set the space width-based tolerance value that is used to estimate where spaces in text should be added. Note
    * that the default value for this has been determined from trial and error. Setting this value larger will reduce
    * the number of spaces added.
    *
    * @param spacingToleranceValue tolerance / scaling factor to use
    */
  def setSpacingTolerance(spacingToleranceValue: Float) {
    spacingTolerance = spacingToleranceValue
  }

  /**
    * Get the current character width-based tolerance value that is being used to estimate where spaces in text should
    * be added. Note that the default value for this has been determined from trial and error.
    *
    * @return The current tolerance / scaling factor
    */
  def getAverageCharTolerance: Float = {
    averageCharTolerance
  }

  /**
    * Set the character width-based tolerance value that is used to estimate where spaces in text should be added. Note
    * that the default value for this has been determined from trial and error. Setting this value larger will reduce
    * the number of spaces added.
    *
    * @param averageCharToleranceValue average tolerance / scaling factor to use
    */
  def setAverageCharTolerance(averageCharToleranceValue: Float) {
    averageCharTolerance = averageCharToleranceValue
  }

  /**
    * returns the multiple of whitespace character widths for the current text which the current line start can be
    * indented from the previous line start beyond which the current line start is considered to be a paragraph start.
    *
    * @return the number of whitespace character widths to use when detecting paragraph indents.
    */
  def getIndentThreshold: Float = {
    indentThreshold
  }

  /**
    * sets the multiple of whitespace character widths for the current text which the current line start can be
    * indented from the previous line start beyond which the current line start is considered to be a paragraph start.
    * The default value is 2.0.
    *
    * @param indentThresholdValue the number of whitespace character widths to use when detecting paragraph indents.
    */
  def setIndentThreshold(indentThresholdValue: Float) {
    indentThreshold = indentThresholdValue
  }

  /**
    * the minimum whitespace, as a multiple of the max height of the current characters beyond which the current line
    * start is considered to be a paragraph start.
    *
    * @return the character height multiple for max allowed whitespace between lines in the same paragraph.
    */
  def getDropThreshold: Float = {
    dropThreshold
  }

  /**
    * sets the minimum whitespace, as a multiple of the max height of the current characters beyond which the current
    * line start is considered to be a paragraph start. The default value is 2.5.
    *
    * @param dropThresholdValue the character height multiple for max allowed whitespace between lines in the same
    *                           paragraph.
    */
  def setDropThreshold(dropThresholdValue: Float) {
    dropThreshold = dropThresholdValue
  }

  /**
    * Returns the string which will be used at the beginning of a paragraph.
    *
    * @return the paragraph start string
    */
  def getParagraphStart: String = {
    paragraphStart
  }

  /**
    * Sets the string which will be used at the beginning of a paragraph.
    *
    * @param s the paragraph start string
    */
  def setParagraphStart(s: String) {
    paragraphStart = s
  }

  /**
    * Returns the string which will be used at the end of a paragraph.
    *
    * @return the paragraph end string
    */
  def getParagraphEnd: String = {
    paragraphEnd
  }

  /**
    * Sets the string which will be used at the end of a paragraph.
    *
    * @param s the paragraph end string
    */
  def setParagraphEnd(s: String) {
    paragraphEnd = s
  }

  /**
    * Returns the string which will be used at the beginning of a page.
    *
    * @return the page start string
    */
  def getPageStart: String = {
    pageStart
  }

  /**
    * Sets the string which will be used at the beginning of a page.
    *
    * @param pageStartValue the page start string
    */
  def setPageStart(pageStartValue: String) {
    pageStart = pageStartValue
  }

  /**
    * Returns the string which will be used at the end of a page.
    *
    * @return the page end string
    */
  def getPageEnd: String = {
    pageEnd
  }

  /**
    * Sets the string which will be used at the end of a page.
    *
    * @param pageEndValue the page end string
    */
  def setPageEnd(pageEndValue: String) {
    pageEnd = pageEndValue
  }

  /**
    * Returns the string which will be used at the beginning of an article.
    *
    * @return the article start string
    */
  def getArticleStart: String = {
    articleStart
  }

  /**
    * Sets the string which will be used at the beginning of an article.
    *
    * @param articleStartValue the article start string
    */
  def setArticleStart(articleStartValue: String) {
    articleStart = articleStartValue
  }

  /**
    * Returns the string which will be used at the end of an article.
    *
    * @return the article end string
    */
  def getArticleEnd: String = {
    articleEnd
  }

  /**
    * Sets the string which will be used at the end of an article.
    *
    * @param articleEndValue the article end string
    */
  def setArticleEnd(articleEndValue: String) {
    articleEnd = articleEndValue
  }

  /**
    * handles the line separator for a new line given the specified current and previous TextPositions.
    *
    * @param current               the current text position
    * @param lastPosition          the previous text position
    * @param lastLineStartPosition the last text position that followed a line separator.
    * @param maxHeightForLine      max height for positions since lastLineStartPosition
    * @return start position of the last line
    * @throws IOException if something went wrong
    */
  @throws(classOf[IOException])
  private def handleLineSeparation(current: PositionWrapper, lastPosition: PositionWrapper, lastLineStartPosition: PositionWrapper, maxHeightForLine: Float): PositionWrapper = {
    current.setLineStart()
    doParagraphSeparation(current, lastPosition, lastLineStartPosition, maxHeightForLine)
    val lastLineStartPosition1 = current
    if (current.isParagraphStart) {
      if (lastPosition.isArticleStart) {
        writeParagraphStart()
      } else {
        writeLineSeparator()
        writeParagraphSeparator()
      }
    } else {
      writeLineSeparator()
    }
    lastLineStartPosition1
  }

  /**
    * tests the relationship between the last text position, the current text position and the last text position that
    * followed a line separator to decide if the gap represents a paragraph separation. This should <i>only</i> be
    * called for consecutive text positions that first pass the line separation test.
    * <p>
    * This base implementation tests to see if the lastLineStartPosition is null OR if the current vertical position
    * has dropped below the last text vertical position by at least 2.5 times the current text height OR if the current
    * horizontal position is indented by at least 2 times the current width of a space character.
    * </p>
    * <p>
    * This also attempts to identify text that is indented under a hanging indent.
    * </p>
    * <p>
    * This method sets the isParagraphStart and isHangingIndent flags on the current position object.
    * </p>
    *
    * @param position              the current text position. This may have its isParagraphStart or isHangingIndent flags set upon
    *                              return.
    * @param lastPosition          the previous text position (should not be null).
    * @param lastLineStartPosition the last text position that followed a line separator, or null.
    * @param maxHeightForLine      max height for text positions since lasLineStartPosition.
    */
  private def doParagraphSeparation(position: PositionWrapper, lastPosition: PositionWrapper, lastLineStartPosition: PositionWrapper, maxHeightForLine: Float):Unit = {
    var result: Boolean = false
    if (lastLineStartPosition == null) {
      result = true
    } else {
      val yGap: Float = Math.abs(position.getTextPosition.getYDirAdj - lastPosition.getTextPosition.getYDirAdj)
      val newYVal: Float = multiplyFloat(getDropThreshold, maxHeightForLine)
      val xGap: Float = position.getTextPosition.getXDirAdj - lastLineStartPosition.getTextPosition.getXDirAdj
      val newXVal: Float = multiplyFloat(getIndentThreshold, position.getTextPosition.getWidthOfSpace)
      val positionWidth: Float = multiplyFloat(0.25f, position.getTextPosition.getWidth)
      if (yGap > newYVal) {
        result = true
      } else if (xGap > newXVal) {
        if (!lastLineStartPosition.isParagraphStart) {
          result = true
        } else {
          position.setHangingIndent()
        }
      } else if (xGap < -position.getTextPosition.getWidthOfSpace) {
        if (!lastLineStartPosition.isParagraphStart) {
          result = true
        }
      } else if (Math.abs(xGap) < positionWidth) {
        if (lastLineStartPosition.isHangingIndent) {
          position.setHangingIndent()
        } else if (lastLineStartPosition.isParagraphStart) {
          val liPattern: Pattern = matchListItemPattern(lastLineStartPosition)
          if (liPattern != null) {
            val currentPattern: Pattern = matchListItemPattern(position)
            if (liPattern eq currentPattern) {
              result = true
            }
          }
        }
      }
    }
    if (result) {
      position.setParagraphStart()
    }
  }

  private def multiplyFloat(value1: Float, value2: Float): Float = {
    (value1 * value2 * 1000).round / 1000f
  }

  /**
    * writes the paragraph separator string to the output.
    *
    * @throws IOException if something went wrong
    */
  @throws(classOf[IOException])
  protected def writeParagraphSeparator() {
    writeParagraphEnd()
    writeParagraphStart()
  }

  /**
    * Write something (if defined) at the start of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  @throws(classOf[IOException])
  protected def writeParagraphStart() {
    if (inParagraph) {
      writeParagraphEnd()
      inParagraph = false
    }
    output.write(getParagraphStart)
    inParagraph = true
  }

  /**
    * Write something (if defined) at the end of a paragraph.
    *
    * @throws IOException if something went wrong
    */
  @throws(classOf[IOException])
  protected def writeParagraphEnd() {
    if (!inParagraph) {
      writeParagraphStart()
    }
    output.write(getParagraphEnd)
    inParagraph = false
  }

  /**
    * Write something (if defined) at the start of a page.
    *
    * @throws IOException if something went wrong
    */
  @throws(classOf[IOException])
  protected def writePageStart() {
    output.write(getPageStart)
  }

  /**
    * Write something (if defined) at the end of a page.
    *
    * @throws IOException if something went wrong
    */
  @throws(classOf[IOException])
  protected def writePageEnd() {
    output.write(getPageEnd)
  }

  /**
    * returns the list item Pattern object that matches the text at the specified PositionWrapper or null if the text
    * does not match such a pattern. The list of Patterns tested against is given by the `getListItemPatterns()`
    * method. To add to the list, simply override that method (if sub-classing) or explicitly supply your own list
    * using `setListItemPatterns(List)`
    *
    * @param pw position
    * @return the matching pattern
    */
  private def matchListItemPattern(pw: PositionWrapper): Pattern = {
    matchPattern(pw.getTextPosition.getUnicode, getListItemPatterns)
  }

  /**
    * a list of regular expressions that match commonly used list item formats, i.e. bullets, numbers, letters, Roman
    * numerals, etc. Not meant to be comprehensive.
    */
  private val LIST_ITEM_EXPRESSIONS: Array[String] = Array("\\.", "\\d+\\.", "\\[\\d+\\]", "\\d+\\)", "[A-Z]\\.", "[a-z]\\.", "[A-Z]\\)", "[a-z]\\)", "[IVXL]+\\.", "[ivxl]+\\.")
  private var listOfPatterns: util.List[Pattern] = null

  /**
    * use to supply a different set of regular expression patterns for matching list item starts.
    *
    * @param patterns list of patterns
    */
  protected def setListItemPatterns(patterns: util.List[Pattern]) {
    listOfPatterns = patterns
  }

  /**
    * returns a list of regular expression Patterns representing different common list item formats. For example
    * numbered items of form:
    * <ol>
    * <li>some text</li>
    * <li>more text</li>
    * </ol>
    * or
    * <ul>
    * <li>some text</li>
    * <li>more text</li>
    * </ul>
    * etc., all begin with some character pattern. The pattern "\\d+\." (matches "1.", "2.", ...) or "\[\\d+\]"
    * (matches "[1]", "[2]", ...).
    * <p>
    * This method returns a list of such regular expression Patterns.
    *
    * @return a list of Pattern objects.
    */
  protected def getListItemPatterns: util.List[Pattern] = {
    if (listOfPatterns == null) {
      listOfPatterns = new util.ArrayList[Pattern]
      for (expression <- LIST_ITEM_EXPRESSIONS) {
        val p: Pattern = Pattern.compile(expression)
        listOfPatterns.add(p)
      }
    }
    listOfPatterns
  }

  /**
    * iterates over the specified list of Patterns until it finds one that matches the specified string. Then returns
    * the Pattern.
    * <p>
    * Order of the supplied list of patterns is important as most common patterns should come first. Patterns should be
    * strict in general, and all will be used with case sensitivity on.
    * </p>
    *
    * @param string   the string to be searched
    * @param patterns list of patterns
    * @return matching pattern
    */
  protected def matchPattern(string: String, patterns: util.List[Pattern]): Pattern = {
    import scala.collection.JavaConversions._
    for (p <- patterns) {
      if (p.matcher(string).matches) {
        return p
      }
    }
    null
  }

  /**
    * Write a list of string containing a whole line of a document.
    *
    * @param line a list with the words of the given line
    * @throws IOException if something went wrong
    */
  @throws(classOf[IOException])
  private def writeLine(line: util.List[WordWithTextPositions]) {
    val numberOfStrings: Int = line.size
    for (i <- 0 until numberOfStrings) {
      val word: WordWithTextPositions = line.get(i)
      writeString(word.getText, word.getTextPositions)
      if (i < numberOfStrings - 1) {
        writeWordSeparator()
      }
    }
  }

  /**
    * Normalize the given list of TextPositions.
    *
    * @param line list of TextPositions
    * @return a list of strings, one string for every word
    */
  private def normalize(line: util.List[LineItem]): util.List[WordWithTextPositions] = {
    val normalized: util.List[WordWithTextPositions] = new util.LinkedList[WordWithTextPositions]
    var lineBuilder: StringBuilder = new StringBuilder
    val wordPositions: util.List[TextPosition] = new util.ArrayList[TextPosition]
    import scala.collection.JavaConversions._
    for (item <- line) {
      lineBuilder = normalizeAdd(normalized, lineBuilder, wordPositions, item)
    }
    if (lineBuilder.nonEmpty) {
      normalized.add(createWord(lineBuilder.toString, wordPositions))
    }
    normalized
  }

  /**
    * Handles the LTR and RTL direction of the given words. The whole implementation stands and falls with the given
    * word. If the word is a full line, the results will be the best. If the word contains of single words or
    * characters, the order of the characters in a word or words in a line may wrong, due to RTL and LTR marks and
    * characters!
    *
    * Based on http://www.nesterovsky-bros.com/weblog/2013/07/28/VisualToLogicalConversionInJava.aspx
    *
    * @param word The word that shall be processed
    * @return new word with the correct direction of the containing characters
    */
  private def handleDirection(word: String): String = {
    val bidi: Bidi = new Bidi(word, Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)
    if (!bidi.isMixed && bidi.getBaseLevel == Bidi.DIRECTION_LEFT_TO_RIGHT) {
      return word
    }
    val runCount: Int = bidi.getRunCount
    val levels: Array[Byte] = new Array[Byte](runCount)
    val runs: Array[Integer] = new Array[Integer](runCount)
    for (i <- 0 until runCount) {
      levels(i) = bidi.getRunLevel(i).toByte
      runs(i) = i
    }
    Bidi.reorderVisually(levels, 0, runs.asInstanceOf[Array[AnyRef]], 0, runCount)
    val result: StringBuilder = new StringBuilder
    for (i <- 0 until runCount) {
      val index: Int = runs(i)
      val start: Int = bidi.getRunStart(index)
      var end: Int = bidi.getRunLimit(index)
      val level: Int = levels(index)
      if ((level & 1) != 0) {
        end -= 1
        while (end >= start) {
          val character: Char = word.charAt(end)
          if (Character.isMirrored(word.codePointAt(end))) {
            if (MIRRORING_CHAR_MAP.containsKey(character)) {
              result.append(MIRRORING_CHAR_MAP.get(character))
            } else {
              result.append(character)
            }
          } else {
            result.append(character)
          }
          end -= 1
        }
      } else {
        result.append(word, start, end)
      }
    }
    result.toString
  }

  private val MIRRORING_CHAR_MAP: util.Map[Character, Character] = new util.HashMap[Character, Character]

  val pathb = "org/apache/pdfbox/resources/text/BidiMirroring.txt"
  val inputb = classOf[PDFFormatStripper].getClassLoader.getResourceAsStream(pathb)
  try {
    parseBidiFile(inputb)
  } catch {
    case e:IOException =>
      log.warn("Could not parse BidiMirroring.txt, mirroring char map will be empty: " + e.getMessage)
  } finally {
    try {
      inputb.close()
    } catch {
      case e:IOException =>
        log.error("Could not close BidiMirroring.txt ", e)
    }
  }

  /**
    * This method parses the bidi file provided as inputstream.
    *
    * @param inputStream - The bidi file as inputstream
    * @throws IOException if any line could not be read by the LineNumberReader
    */
  @throws(classOf[IOException])
  private def parseBidiFile(inputStream: InputStream) {
    val rd: LineNumberReader = new LineNumberReader(new InputStreamReader(inputStream))
    do {
      var s: String = rd.readLine
      if (s == null) {
        return
      }
      val comment: Int = s.indexOf('#')
      if (comment != -1) {
        s = s.substring(0, comment)
      }
      if (s.length >= 2) {
        val st: StringTokenizer = new StringTokenizer(s, ";")
        val nFields: Int = st.countTokens
        val fields: Array[Character] = new Array[Character](nFields)
        for (i <- 0 until nFields) {
          fields(i) = Integer.parseInt(st.nextToken.trim, 16).toChar
        }
        if (fields.length == 2) {
          MIRRORING_CHAR_MAP.put(fields(0), fields(1))
        }
      }
    } while (true)
  }

  /**
    * Used within `normalize(List, boolean, boolean)` to create a single `WordWithTextPositions` entry.
    */
  private def createWord(word: String, wordPositions: util.List[TextPosition]): WordWithTextPositions = {
    new WordWithTextPositions(normalizeWord(word), wordPositions)
  }

  /**
    * Normalize certain Unicode characters. For example, convert the single "fi" ligature to "f" and "i". Also
    * normalises Arabic and Hebrew presentation forms.
    *
    * @param word Word to normalize
    * @return Normalized word
    */
  private def normalizeWord(word: String): String = {
    var builder: StringBuilder = null
    var p: Int = 0
    var q: Int = 0
    val strLength: Int = word.length
    for (i <- 0 until strLength) {
      q = i
      val c: Char = word.charAt(i)
      if (0xFB00 <= c && c <= 0xFDFF || 0xFE70 <= c && c <= 0xFEFF) {
        if (builder == null) {
          builder = new StringBuilder(strLength * 2)
        }
        builder.append(word.substring(p, i))
        if (c == 0xFDF2 && i > 0 && (word.charAt(i - 1) == 0x0627 || word.charAt(i - 1) == 0xFE8D)) {
          builder.append("\u0644\u0644\u0647")
        } else {
          builder.append(Normalizer.normalize(word.substring(i, i + 1), Normalizer.Form.NFKC).trim)
        }
        p = i + 1
      }
    }
    if (builder == null) {
      handleDirection(word)
    } else {
      builder.append(word.substring(p, q))
      handleDirection(builder.toString)
    }
  }

  /**
    * Used within `normalize(List, boolean, boolean)` to handle a `TextPosition`.
    *
    * @return The StringBuilder that must be used when calling this method.
    */
  private def normalizeAdd(normalized: util.List[WordWithTextPositions], lineBuilder: StringBuilder, wordPositions: util.List[TextPosition], item: LineItem): StringBuilder = {
    var ret: StringBuilder = lineBuilder
    if (item.isWordSeparator) {
      normalized.add(createWord(ret.toString, new util.ArrayList[TextPosition](wordPositions)))
      ret = new StringBuilder
      wordPositions.clear()
    } else {
      val text: TextPosition = item.getTextPosition
      ret.append(text.getUnicode)
      wordPositions.add(text)
    }
    ret
  }

  /**
    * internal marker class. Used as a place holder in a line of TextPositions.
    */
  private object LineItem {

    var WORD_SEPARATOR: LineItem = new LineItem

    def getWordSeparator: LineItem = {
      WORD_SEPARATOR
    }
  }

  private final class LineItem(private var textPosition: TextPosition = null) {

    def this(){
      this(null)
    }

    def getTextPosition: TextPosition = {
      textPosition
    }

    def isWordSeparator: Boolean = {
      textPosition == null
    }
  }

  /**
    * Internal class that maps strings to lists of `TextPosition` arrays. Note that the number of entries in that
    * list may differ from the number of characters in the string due to normalization.
    *
    * @author Axel Dörfler
    */
  private final class WordWithTextPositions(
    private var text: String = null,
    private var textPositions: util.List[TextPosition] = null
                                           ) {
    def getText: String = {
      text
    }

    def getTextPositions: util.List[TextPosition] = {
      textPositions
    }
  }

  /**
    * wrapper of TextPosition that adds flags to track status as linestart and paragraph start positions.
    * <p>
    * This is implemented as a wrapper since the TextPosition class doesn't provide complete access to its state fields
    * to subclasses. Also, conceptually TextPosition is immutable while these flags need to be set post-creation so it
    * makes sense to put these flags in this separate class.
    * </p>
    *
    * @author m.martinez@ll.mit.edu
    */
  private final class PositionWrapper(private var position: TextPosition = null) {
    private var isLineStartTag: Boolean = false
    private var isParagraphStartTag: Boolean = false
    private var isPageBreakTag: Boolean = false
    private var isHangingIndentTag: Boolean = false
    private var isArticleStartTag: Boolean = false

    /**
      * Returns the underlying TextPosition object.
      *
      * @return the text position
      */
    def getTextPosition: TextPosition = {
      position
    }

    def isLineStart: Boolean = {
      isLineStartTag
    }

    /**
      * Sets the isLineStart() flag to true.
      */
    def setLineStart() {
      this.isLineStartTag = true
    }

    def isParagraphStart: Boolean = {
      isParagraphStartTag
    }

    /**
      * sets the isParagraphStart() flag to true.
      */
    def setParagraphStart() {
      this.isParagraphStartTag = true
    }

    def isArticleStart: Boolean = {
      isArticleStartTag
    }

    /**
      * Sets the isArticleStart() flag to true.
      */
    def setArticleStart() {
      this.isArticleStartTag = true
    }

    def isPageBreak: Boolean = {
      isPageBreakTag
    }

    /**
      * Sets the isPageBreak() flag to true.
      */
    def setPageBreak() {
      this.isPageBreakTag = true
    }

    def isHangingIndent: Boolean = {
      isHangingIndentTag
    }

    /**
      * Sets the isHangingIndent() flag to true.
      */
    def setHangingIndent() {
      this.isHangingIndentTag = true
    }
  }

}
