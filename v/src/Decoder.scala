package v

case class OP(tpe: String, funct6: String, v: Boolean, x: Boolean, i: Boolean, f: Boolean, name: String)

case class SpecialOp(name: String, vs: String, ops: Seq[(String, String)])

object Decoder extends App {
  val instTable = os.read(os.resource() / "inst-table.adoc").split("<<<")
  val normalTable = instTable.head
  val specialTable = instTable.last
  val pattern = raw"\| (\d{6})* *\|([V ])\|([X ])\|([I ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([X ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([F ])\| *([\w.<>/]*)".r
  val ops = normalTable.split("\n").flatMap {
    case pattern(opiFunct6, opiV, opiX, opiI, opiName, opmFunct6, opmV, opmX, opmName, opfFunct6, opfV, opfF, opfName) => Seq(
      if (opiName.nonEmpty) Some(OP("I", opiFunct6, opiV == "V", opiX == "X", opiI == "I", f = false, opiName)) else None,
      if (opmName.nonEmpty) Some(OP("M", opmFunct6, opmV == "V", opmX == "X", i = false, f = false, opmName)) else None,
      if (opfName.nonEmpty) Some(OP("F", opfFunct6, opfV == "V", x = false, i = false, f = opfF == "F", opfName)) else None,
    ).flatten
    case _ => Seq.empty
  }
  val specialOps: Seq[SpecialOp] = specialTable.split(raw"\n\.").drop(1).map { str =>
    val namePattern = raw"(\w+) encoding space".r
    val vsPattern = raw"\| *vs(\d) *\|.*".r
    val opPattern = raw"\| *(\d{5}) *\| *(.*)".r
    val lines = str.split("\n")
    val name = lines.collectFirst { case namePattern(name) => name }.get
    val vs = lines.collectFirst { case vsPattern(vs) => vs }.get
    val ops = lines.collect { case opPattern(op, name) => (op, name) }
    SpecialOp(name, vs, ops)
  }
}