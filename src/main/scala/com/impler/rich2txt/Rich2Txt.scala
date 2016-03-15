package com.impler.rich2txt

import org.apache.logging.log4j.{LogManager, Logger}

/**
  * Rich2Txt
  */
object Rich2Txt {

  private final val logger: Logger= LogManager.getLogger(getClass)

  def main(args : Array[String]): Unit = {
    val appHome = System.getenv("APP_HOME")
    logger.info("APP_HOME = {}",appHome)
  }

}
