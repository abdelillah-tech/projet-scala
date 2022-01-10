package fr.esgi.al.funprog

import fr.esgi.al.funprog.model._
import fr.esgi.al.funprog._

object Runner {
  def run(): Unit = {
    val data: List[List[String]] = Parseur.readInfos()
    val parsedData: ParsedData = Parseur.parser(data)
    val tendeuses: List[Tendeuse] = Controleur.designate(parsedData)
    val result: Result = Result(parsedData.limit, tendeuses)

    Exporteur.writeResultJsonFile(Writes.of[Result].writesJson(result).toString)
    Exporteur.writeResultCsvFile(Writes.of[Result].writesCsv(result))
    Exporteur.writeResultYamlFile(Writes.of[Result].writesYaml(result, ""))
  }
}
