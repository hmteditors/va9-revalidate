import edu.holycross.shot.mid.validator._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.cex._
import scala.io.Source
import java.io.PrintWriter

import org.homermultitext.edmodel._
import edu.holycross.shot.greek._

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
val reporter = ValidationReporter(midValidator)

// Unspeakable kludge of a script, but the output is pretty.
def twinScholia(e3String: String, msBString: String) = {
  println("Making comparison of two pages...")


  val msBurn = Cite2Urn(msBString)
  val msBcorpus = reporter.corpusForPage(msBurn)
  val msBurns = msBcorpus.nodes.map(_.urn)

  val e3urn = Cite2Urn(e3String)
  val e3corpus = reporter.corpusForPage(e3urn)
  val e3urns = e3corpus.nodes.map(_.urn)

  val e3DseReporter =  DseReporter(e3urn, midValidator.dse, e3corpus, midValidator.readers)
  val msBDseReporter =  DseReporter(msBurn, midValidator.dse, reporter.corpusForPage(msBurn), midValidator.readers)

  val baseDir = File("validation")
  val fName = e3urn.collection + "-" + e3urn.objectComponent + "-" + msBurn.collection+ "-" + msBurn.objectComponent + ".md"
  val outFile = baseDir/fName

  val pairings=  DataCollector.compositeFiles("relations", "cex", 1).split("\n").filter(_.nonEmpty)



  val rows = for (pr <- pairings) yield {
    val urns = pr.split("#").toVector

    val e3Str = if (urns(0).isEmpty) {
      ""
    } else {
      val scholion = CtsUrn(urns(0))
      if (e3urns.contains(scholion)) {
        e3DseReporter.passageMarkdown(scholion)
      } else { "" }

    }
    val msB = if (urns.size == 1) {

      ""
    } else {
      val scholion = CtsUrn(urns(1))
      if (msBurns.contains(scholion)) {
        msBDseReporter.passageMarkdown(scholion)
      } else { "" }

    }
    if ((msB + e3Str).isEmpty) {
      ""
    } else {
      "| " + e3Str + " | " + msB + " |"
    }
  }
  val hdr = "| Upsilon 1.1 | Venetus B |\n|:-----------|:-----------|\n"
  val md = hdr + rows.filter(_.nonEmpty).mkString("\n")
  //new PrintWriter("parallel-scholia.md"){write(md);close;}
  outFile.overwrite(md)
}

  def namesAuthority :  Map[Cite2Urn, String]= {
    val lines = scala.io.Source.fromURL("https://raw.githubusercontent.com/homermultitext/hmt-authlists/master/data/hmtnames.cex").getLines.toVector.drop(2)

    val auths = for (ln <- lines) yield {
      val cols = ln.split("#")
      (Cite2Urn(cols(0)) -> cols(3))
    }
    auths.toMap
  }

def validatePNs(uString: String) = {
  val pg = Cite2Urn(uString)

  val corpus = reporter.corpusForPage(pg)
  //println("SIZE OF CORPUS: " + corpus.size + " NODEs.")
  val rept = StringBuilder.newBuilder
  rept.append("# Named entity verification: " + pg + "\n\n")
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
    //println(s"${u} ${persAuth(u)}")
    rept.append(s"### ${persAuth(u)} == ${u} \n\n")
    val matches = peopleList.filter(_.lexicalDisambiguation == u)
    for (tkn <- matches) {
      rept.append("-  " + tkn.editionUrn + " " + tkn.leidenDiplomatic + "\n")
      //println("\t" + tkn.editionUrn + " " + tkn.leidenDiplomatic)
    }
    rept.append("\n\n")
  }

  val baseDir = File(s"validation/${pg.collection}-${pg.objectComponent}")
  //val fName = e3urn.collection + "-" + e3urn.objectComponent + "-" + msBurn.collection+ "-" + msBurn.objectComponent + ".md"
  val reptName = "personal-names.md"
  val outFile = baseDir/reptName
  outFile.overwrite(rept.toString)
}

def validate(uString : String) = {
  reporter.validate(uString)
  validatePNs(uString)
}


def validatePair(e3String : String, msBString: String) = {
  reporter.validate(e3String)
  reporter.validate(msBString)
  twinScholia(e3String,msBString)
}


println("\n\nValidate editorial work for a given page:")
println("\n\tvalidate(\"PAGEURN\")\n\n")
println("\n\nValidate editorial work for a related pair of pages\nin Upsilon 1.1 and Venetus B:")
println("\n\tvalidatePair(\"UPSILON-URN\", \"VB-URN\")\n\n")
