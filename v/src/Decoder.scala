package v

import chisel3.util.BitPat

import scala.util.matching.Regex

case class Op(tpe: String, funct6: String, funct3s: Seq[String], name: String)

case class SpecialOp(name: String, vs: String, ops: Seq[(String, String)])

case class SpecialAux(name: String, vs: Int, value: String)

object Decoder {
  val instTable: Array[String] = os.read(os.resource() / "inst-table.adoc").split("<<<")
  val normalTable: String = instTable.head
  val specialTable: String = instTable.last
  val pattern: Regex = raw"\| (\d{6})* *\|([V ])\|([X ])\|([I ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([X ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([F ])\| *([\w.<>/]*)".r
  val ops: Array[Op] = normalTable.split("\n").flatMap {
    case pattern(opiFunct6, opiV, opiX, opiI, opiName, opmFunct6, opmV, opmX, opmName, opfFunct6, opfV, opfF, opfName) => Seq(
      if (opiName.nonEmpty) Some(Op("I", opiFunct6, Seq(opiV, opiX, opiI), opiName)) else None,
      if (opmName.nonEmpty) Some(Op("M", opmFunct6, Seq(opmV, opmX), opmName)) else None,
      if (opfName.nonEmpty) Some(Op("F", opfFunct6, Seq(opfV, opfF), opfName)) else None,
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

object InstructionDecodeTable {
  def expand(op: Op, specialOps: Seq[SpecialOp]): Seq[(Op, Option[SpecialAux])] = {
    if (op.name.startsWith("v")) return Seq((op, None))

    val sp = specialOps.find(_.name == op.name).get

    sp.ops.map(x => {
      (op.copy(name = x._2), Some(SpecialAux(sp.name, sp.vs.toInt, x._1)))
    })
  }

  val funct3Map: Map[String, String] = Map(
    "IV" -> "000",
    "IX" -> "100",
    "II" -> "011",
    "MV" -> "010",
    "MX" -> "110",
    "FV" -> "001",
    "FF" -> "101",
  )

  def keys(op: Op, aux: Option[SpecialAux]): Seq[String] = {
    op.funct3s.filter(_ != " ").map(x =>
      op.funct6 +                                  // funct6
      "?" +                                        // always '?', but why?
      (if (aux.isEmpty || aux.get.vs == 1) "?????"
      else aux.get.value) +                        // vs2
      (if (aux.isEmpty || aux.get.vs == 2) "?????"
      else aux.get.value) +                        // vs1
      funct3Map(op.tpe + x)                        // funct3
    )
  }

  def values(op: Op, special: Option[SpecialAux]): Seq[String] = {
    op.funct3s.filter(_ != " ").map(x => value(op, special, x).toString)
  }

  val logic: Seq[String] = Seq(
    "and", "or"
  )
  val add: Seq[String] = Seq(
    "add", "sub", "slt", "sle", "sgt", "sge",
    "max", "min", "seq", "sne", "adc", "sbc", "sum"
  )
  val shift: Seq[String] = Seq(
    "srl", "sll", "sra"
  )
  val mul: Seq[String] = Seq(
    "mul", "madd", "macc", "msub", "msac"
  )
  val mul2: Seq[String] = Seq(
    "mul", "ma", "ms"
  )
  val div: Seq[String] = Seq(
    "div", "rem"
  )
  val other: Seq[String] = Seq(
    "slide", "rgather", "merge", "mv", "clip", "compress"
  )
  val ffo: Seq[String] = Seq(
    "vfirst", "vmsbf", "vmsof", "vmsif"
  )

  case class Value(units: String, uop: String, controls: String,
                 v: Boolean, x: Boolean, i: Option[Boolean]) {
    require(units.length == 6)
    require(uop.length == 4)
    require(controls.length == 12)

    override def toString: String = units + controls +
      (if (v) "1" else "0") +
      (if (x) "1" else "0") +
      (if (i.isEmpty) "?" else if (i.get) "1" else "0") +
      uop
  }

  def value(op: Op, special: Option[SpecialAux], funct3: String): Value = {
    val b2s = (b: Boolean) => if (b) "1" else "0"
    val firstIndexContains = (xs: Iterable[String], s: String) =>
      xs.map(s.indexOf).zipWithIndex.filter(_._1 != -1).head._2

    val logicUnit = logic.exists(op.name.contains)
    val addUnit = add.exists(op.name.contains) && !(op.tpe == "M" && Seq("vm", "vnm").exists(op.name.startsWith))
    val shiftUnit = shift.exists(op.name.contains)
    val mulUnit = mul.exists(op.name.contains)
    val divUnit = div.exists(op.name.contains)
    val otherUnit = other.exists(op.name.contains)
    val ffoUnit = ffo.contains(op.name)
    val units = if (special.isEmpty) {
      b2s(logicUnit) + b2s(addUnit) + b2s(shiftUnit) + b2s(mulUnit) + b2s(divUnit) + b2s(otherUnit)
    } else "000001"
    val uop = if (special.isEmpty) {
      if (mulUnit){
        val high = op.name.contains("mulh")
        val n = if (high) 3 else firstIndexContains(mul2, op.name)
        require(n < 4)
        b2s(op.name.startsWith("vn")) + b2s(Seq("c", "cu", "cus", "csu").exists(op.name.endsWith)) +
          ("00" + n.toBinaryString takeRight 2)
      } else if (divUnit) {
        val n = firstIndexContains(div, op.name)
        require(n < 2)
        "?"*3 + n.toBinaryString
      } else if (addUnit) {
        val n = if (op.name.contains("sum")) 0 else firstIndexContains(add, op.name)
        require(n < 16)
        ("0000" + n.toBinaryString takeRight 4)
      } else if (logicUnit) {
        val isXnor = op.name == "vmxnor"
        val isXor = op.name.contains("xor")
        val n = if (isXnor || isXor) 2 else firstIndexContains(logic, op.name)
        require(n < 4)
        b2s(op.name.startsWith("vmn")) +
          b2s(isXnor || op.name.contains("not")) +
          ("00" + n.toBinaryString takeRight 2)
      } else if (shiftUnit) {
        val n = firstIndexContains(shift, op.name)
        require(n < 4)
        "?"*2 + ("00" + n.toBinaryString takeRight 2)
      } else if (otherUnit) {
        val n =firstIndexContains(other, op.name)
        require(n < 8)
        "0" + ("000" + n.toBinaryString takeRight 3)
      } else {
        // unreachable
        require(false)
        "?"*4
      }
    } else
      "1" + (
        if (ffoUnit)
          "?" +
            ("00" + ffo.indexOf(op.name).toBinaryString takeRight 2)
        else if (special.get.name == "VXUNARY0") {
          val log2 = (x: Int) => (math.log10(x)/math.log10(2)).toInt
          b2s(op.name.startsWith("vs")) +
            ("00" + log2(op.name.last.toString.toInt).toBinaryString takeRight 2)
        } else
          "?"*3
        )

    val nameWoW = op.name.replace(".w", "")
    val controls = if (special.isEmpty)
      b2s(op.name.endsWith(".w")) +
        b2s(op.name.contains("ei16")) +
        b2s(op.name.contains("<nr>")) +
        b2s(op.name.contains("red")) +
        b2s(op.name.startsWith("vm") && ((addUnit && !Seq("min", "max").exists(op.name.contains)) || logicUnit)) +
        b2s(op.name == "vrsub") +
        b2s(op.name.startsWith("vn") && (shiftUnit || otherUnit)) +
        b2s(op.name.startsWith("vw")) +
        b2s(Seq("vsa", "vss", "vsm").exists(op.name.startsWith)) +
        b2s(Seq("vaa", "vas").exists(op.name.startsWith)) +
        b2s(nameWoW.endsWith("us") || (nameWoW.endsWith("u") && !nameWoW.endsWith("su"))) +
        b2s(nameWoW.endsWith("u"))
    else
      "?"*4 +
        b2s(special.get.name == "VWXUNARY0") +
        b2s(special.get.name == "VXUNARY0") +
        b2s(op.name.startsWith("vmv")) +
        b2s(ffoUnit) +
        b2s(op.name == "vpopc") +
        b2s(op.name == "viota") +
        b2s(op.name == "vid") +
        b2s(funct3 == "V")
    Value(
      units, uop, controls,
      v = funct3 == "V",
      x = funct3 == "X",
      i = if (special.nonEmpty) None else Some(funct3 == "I"),
    )
  }

  def table: List[(BitPat, BitPat)] = {
      Decoder.ops.zipWithIndex.map { x =>
      // vmv<nr>r's funct6 is empty and needs special handling.
      if (x._1.name == "vmv<nr>r") x._1.copy(funct6 = Decoder.ops(x._2-1).funct6)
      else x._1
    }
    // TODO: floating point instructions are not supported for now.
    .filter(_.tpe != "F").flatMap(expand(_, Decoder.specialOps)).flatMap(
      x => keys(x._1, x._2).zip(values(x._1, x._2))
    ).map(x => (BitPat("b" + x._1), BitPat("b" + x._2))).toList
  }
}
