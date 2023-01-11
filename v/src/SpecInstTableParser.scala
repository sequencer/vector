package v

import chisel3.util.BitPat
import chisel3.util.experimental.decode._

import scala.util.matching.Regex

// TODO: refactor String to detail type.
case class RawOp(tpe: String, funct6: String, funct3s: Seq[String], name: String)
case class SpecialAux(name: String, vs: Int, value: String)
case class Op(tpe: String, funct6: String, funct3: String,
              name: String, special: Option[SpecialAux]) extends DecodePattern {
  val funct3Map: Map[String, String] = Map(
    "IV" -> "000",
    "IX" -> "100",
    "II" -> "011",
    "MV" -> "010",
    "MX" -> "110",
    "FV" -> "001",
    "FF" -> "101"
  )

  def bitPat: BitPat = BitPat(
    "b" +
      // funct6
      funct6 +
      // TODO[0]: ? for vm
      "?" +
      // vs2
      (if (special.isEmpty || special.get.vs == 1) "?????" else special.get.value) +
      // vs1
      (if (special.isEmpty || special.get.vs == 2) "?????" else special.get.value) +
      // funct3
      funct3Map(tpe + funct3)
  )
}

/** Parser for inst-table.adoc. */
object SpecInstTableParser {
  val instTable:    Array[String] = os.read(os.resource() / "inst-table.adoc").split("<<<")
  val normalTable:  String = instTable.head
  val specialTable: String = instTable.last
  val pattern: Regex =
    raw"\| (\d{6})* *\|([V ])\|([X ])\|([I ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([X ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([F ])\| *([\w.<>/]*)".r
  val rawOps: Array[RawOp] = normalTable.split("\n").flatMap {
    case pattern(
    opiFunct6,
    opiV,
    opiX,
    opiI,
    opiName,
    opmFunct6,
    opmV,
    opmX,
    opmName,
    opfFunct6,
    opfV,
    opfF,
    opfName
    ) =>
      Seq(
        if (opiName.nonEmpty) Some(RawOp("I", opiFunct6, Seq(opiV, opiX, opiI), opiName)) else None,
        if (opmName.nonEmpty) Some(RawOp("M", opmFunct6, Seq(opmV, opmX), opmName)) else None,
        if (opfName.nonEmpty) Some(RawOp("F", opfFunct6, Seq(opfV, opfF), opfName)) else None
      ).flatten
    case _ => Seq.empty
  }

  val expandedOps: Array[Op] = rawOps.flatMap { rawOp =>
    rawOp.funct3s
      .filter(_ != " ")
      .map(funct3 =>
        Op(
          rawOp.tpe,
          rawOp.funct6,
          funct3,
          rawOp.name,
          None
        )
      )
  }

  val ops: Array[Op] =
    (expandedOps.filter(!_.name.startsWith("V")) ++ specialTable.split(raw"\n\.").drop(1).flatMap { str =>
      val namePattern = raw"(\w+) encoding space".r
      val vsPattern = raw"\| *vs(\d) *\|.*".r
      val opPattern = raw"\| *(\d{5}) *\| *(.*)".r
      val lines = str.split("\n")
      val name = lines.collectFirst { case namePattern(name) => name }.get
      val vs = lines.collectFirst { case vsPattern(vs) => vs }.get.toInt
      val specialOps = lines.collect { case opPattern(op, name) => (op, name) }

      expandedOps.filter(_.name.startsWith("V")).flatMap { op =>
        if (op.name == name) {
          specialOps.map(sp => op.copy(name = sp._2, special = Some(SpecialAux(name, vs, sp._1))))
        } else
          Array.empty[Op]
      }
      // filter out F instructions, for now.
    }).filter(_.tpe != "F")
}
