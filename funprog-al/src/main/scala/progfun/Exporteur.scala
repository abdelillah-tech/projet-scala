package fr.esgi.al.funprog

import com.typesafe.config.{Config, ConfigFactory}
import better.files._

object Exporteur {

  def writeResultJsonFile(text: String): Unit = {
    val conf: Config = ConfigFactory.load()
    val path: String = conf.getString("application.output-json-file")

    val f = File(path)

    f.createIfNotExists()
      .overwrite("")
      .appendLine(text)
    ()
  }

  def writeResultCsvFile(text: String): Unit = {
    val conf: Config = ConfigFactory.load()
    val path: String = conf.getString("application.output-csv-file")

    val f = File(path)

    f.createIfNotExists()
      .overwrite("")
      .appendLine(text)
    ()
  }

  def writeResultYamlFile(text: String): Unit = {
    val conf: Config = ConfigFactory.load()
    val path: String = conf.getString("application.output-yaml-file")

    val f = File(path)

    f.createIfNotExists()
      .overwrite("")
      .appendLine(text)
    ()
  }
}
