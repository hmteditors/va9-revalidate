import edu.holycross.shot.mid.validator._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.cex._
//import scala.io.Source
import java.io.PrintWriter

import org.homermultitext.edmodel._
import edu.holycross.shot.greek._

import edu.holycross.shot.citebinaryimage._

import better.files._
import File._
import java.io.{File => JFile}
import better.files.Dsl._

import scala.xml._

val readerMap =   Map(
  "DiplomaticReader" ->   Vector(DiplomaticReader)
)

val orthoMap = Map(
  "LiteraryGreekString" -> LiteraryGreekString
)

def readersForString(readerName: String): Vector[MidMarkupReader] = {
  if (readerMap.keySet.contains(readerName)){
    readerMap(readerName)
  } else {
    throw (new Exception(s"${readerName} is not a recognized MidReader in this project."))
  }
}

def orthoForString(orthoName: String): MidOrthography = {
  if (orthoMap.keySet.contains(orthoName)){
    orthoMap(orthoName)
  } else {
    throw (new Exception(s"${orthoName} is not a recognized Orthography in this project."))
  }
}

def readerMappings(csvSource : String = "editions/readers.csv") = {
  // drop header row:
  val csvRows = scala.io.Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    ReadersPairing(CtsUrn(parts(0)), readersForString(parts(1)))
  }
  pairs.toVector
}

def orthoMappings(csvSource : String = "editions/orthographies.csv") = {
  val csvRows = scala.io.Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    OrthoPairing(CtsUrn(parts(0)), orthoForString(parts(1)))
  }
  pairs.toVector
}


val repo = EditorsRepo(".")
val midValidator = Validator(repo, readerMappings(), orthoMappings())
val dse = midValidator.dse
val reporter = ValidationReporter(midValidator)

def imagePathForMSCollection(msColl: String): String = {
  msColl match {
    case "msA" => "/project/homer/pyramidal/deepzoom/hmt/vaimg/2017a/"
    case "msB" => "/project/homer/pyramidal/deepzoom/hmt/vbbifolio/v1/"
    case "e3" => "/project/homer/pyramidal/deepzoom/hmt/e3bifolio/v1/"
  }
}



// Map of Perosnal names URNs to string labels
def namesAuthority :  Map[Cite2Urn, String]= {
  val lines = scala.io.Source.fromURL("https://raw.githubusercontent.com/homermultitext/hmt-authlists/master/data/hmtnames.cex").getLines.toVector.drop(2)

  val auths = for (ln <- lines) yield {
    val cols = ln.split("#")
    (Cite2Urn(cols(0)) -> cols(3))
  }
  auths.toMap
}


// Map of place name URNs to string labels
def placesAuthority :  Map[Cite2Urn, String]= {
  val lines = scala.io.Source.fromURL("https://raw.githubusercontent.com/homermultitext/hmt-authlists/master/data/hmtplaces.cex").getLines.toVector.drop(2)

  val auths = for (ln <- lines) yield {
    val cols = ln.split("#")
    (Cite2Urn(cols(0)) -> cols(1))
  }
  auths.toMap
}


// Write report for named entity validation.
def validatePNs(uString: String) = {
  val pg = Cite2Urn(uString)

  val corpus = reporter.corpusForPage(pg)
  val rept = StringBuilder.newBuilder
  rept.append("# Named entity verification for people: " + pg + "\n\n")
  val allPeople = for (nd <- corpus.nodes) yield {
    val n = XML.loadString(nd.text)
    val settings = TokenSettings(nd.urn, LexicalToken)
    val tokens = TeiReader.collectTokens(n, settings)
    val people = tokens.filter(_.lexicalDisambiguation ~~ Cite2Urn("urn:cite2:hmt:pers:"))
    people
  }
  val peopleList = allPeople.flatten
  println(peopleList.size + " Name tokens ")
  val persAuth = namesAuthority
  val persUrns = peopleList.map(_.lexicalDisambiguation)

  val ok = persUrns.filter(persUrns.contains(_))
  if (ok.size != persUrns.size) {
    rept.append("## Errors\n\n")
    rept.append("There were errors in personal name identifiers.\n\n")
    val badList = persUrns.filterNot(persUrns.contains(_))
    for (bad <- badList) {
      rept.append("-  " + bad + " not found in authority list.\n")
    }
    rept.append("\n")

  } else {
    rept.append("All personal name identifiers were found in authority list.\n\n")
  }

  rept.append("## Verification\n\n")
  for (u <- persUrns.distinct) {
    rept.append(s"### ${persAuth(u)} (*${u.objectComponent}*) \n\n")
    val matches = peopleList.filter(_.lexicalDisambiguation == u)
    for (tkn <- matches) {
      rept.append("-  " + tkn.editionUrn + " " + tkn.leidenDiplomatic + "\n")
    }
    rept.append("\n\n")
  }

  val baseDir = File(s"validation/${pg.collection}-${pg.objectComponent}")

  val reptName = "personal-names.md"
  val outFile = baseDir/reptName
  outFile.overwrite(rept.toString)
}

// Write report for validation of place names.
def validatePlaces(uString: String) = {
  val pg = Cite2Urn(uString)
  val rept = StringBuilder.newBuilder
  rept.append("# Named entity verification for places: " + pg + "\n\n")

  val corpus = reporter.corpusForPage(pg)
  val allPlaces = for (nd <- corpus.nodes) yield {
    val n = XML.loadString(nd.text)
    val settings = TokenSettings(nd.urn, LexicalToken)
    val tokens = TeiReader.collectTokens(n, settings)
    val places = tokens.filter(_.lexicalDisambiguation ~~ Cite2Urn("urn:cite2:hmt:place:"))
    places
  }
  val placeList = allPlaces.flatten
  val placeUrns = placeList.map(_.lexicalDisambiguation)
  println(placeList.size + " Place tokens ")
  val placeAuth = placesAuthority
  //val placeUrns = placeList.map(_.lexicalDisambiguation)

  val ok = placeUrns.filter(placeUrns.contains(_))
  if (ok.size != placeUrns.size) {
    rept.append("## Errors\n\n")
    rept.append("There were errors in place name identifiers.\n\n")
    val badList = placeUrns.filterNot(placeUrns.contains(_))
    for (bad <- badList) {
      rept.append("-  " + bad + " not found in authority list.\n")
    }
    rept.append("\n")

  } else {
    rept.append("All place name identifiers were found in authority list.\n\n")
  }

  rept.append("## Verification\n\n")
  for (u <- placeUrns.distinct) {
    //println(s"${u} ${persAuth(u)}")
    rept.append(s"### ${placeAuth(u)} (*${u.objectComponent}*) \n\n")
    val matches = placeList.filter(_.lexicalDisambiguation == u)
    for (tkn <- matches) {
      rept.append("-  " + tkn.editionUrn + " " + tkn.leidenDiplomatic + "\n")
      //println("\t" + tkn.editionUrn + " " + tkn.leidenDiplomatic)
    }
    rept.append("\n\n")
  }

  val baseDir = File(s"validation/${pg.collection}-${pg.objectComponent}")
  //val fName = e3urn.collection + "-" + e3urn.objectComponent + "-" + msBurn.collection+ "-" + msBurn.objectComponent + ".md"
  val reptName = "place-names.md"
  val outFile = baseDir/reptName
  outFile.overwrite(rept.toString)
}


// Validate titles
// validate urns in  scholia references



def validate(uString : String) = {
  reporter.validate(uString)
  validatePNs(uString)
  validatePlaces(uString)

}




println("\n\nValidate editorial work for a given page:")
println("\n\tvalidate(\"PAGEURN\")\n\n")
