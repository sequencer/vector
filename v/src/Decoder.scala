package v

import chisel3.util.BitPat
import chisel3.util.experimental.decode.TruthTable

import scala.collection.immutable.SeqMap
import scala.util.matching.Regex

case class RawOp(tpe: String, funct6: String, funct3s: Seq[String], name: String)

case class SpecialAux(name: String, vs: Int, value: String)
case class Op(tpe: String, funct6: String, funct3: String, name: String, special: Option[SpecialAux]) {
  val funct3Map: Map[String, String] = Map(
    "IV" -> "000",
    "IX" -> "100",
    "II" -> "011",
    "MV" -> "010",
    "MX" -> "110",
    "FV" -> "001",
    "FF" -> "101",
  )

  def bitPat: BitPat = BitPat("0b" +
      // funct6
      funct6 +
      // always '?', but why?
      "?" +
      // vs2
      (if (special.isEmpty || special.get.vs == 1) "?????" else special.get.value) +
      // vs1
      (if (special.isEmpty || special.get.vs == 2) "?????" else special.get.value) +
      // funct3
      funct3Map(tpe + funct3)
    )
}

object Decoder {
  val instTable: Array[String] = os.read(os.resource() / "inst-table.adoc").split("<<<")
  val normalTable: String = instTable.head
  val specialTable: String = instTable.last
  val pattern: Regex = raw"\| (\d{6})* *\|([V ])\|([X ])\|([I ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([X ])\| *([\w.<>/]*) *\| (\d{6})* *\|([V ])\|([F ])\| *([\w.<>/]*)".r
  val rawOps: Array[RawOp] = normalTable.split("\n").flatMap {
    case pattern(opiFunct6, opiV, opiX, opiI, opiName, opmFunct6, opmV, opmX, opmName, opfFunct6, opfV, opfF, opfName) => Seq(
      if (opiName.nonEmpty) Some(RawOp("I", opiFunct6, Seq(opiV, opiX, opiI), opiName)) else None,
      if (opmName.nonEmpty) Some(RawOp("M", opmFunct6, Seq(opmV, opmX), opmName)) else None,
      if (opfName.nonEmpty) Some(RawOp("F", opfFunct6, Seq(opfV, opfF), opfName)) else None,
    ).flatten
    case _ => Seq.empty
  }

  val ops: Array[Op] = specialTable.split(raw"\n\.").drop(1).flatMap { str =>
    val namePattern = raw"(\w+) encoding space".r
    val vsPattern = raw"\| *vs(\d) *\|.*".r
    val opPattern = raw"\| *(\d{5}) *\| *(.*)".r
    val lines = str.split("\n")
    val name = lines.collectFirst { case namePattern(name) => name }.get
    val vs = lines.collectFirst { case vsPattern(vs) => vs }.get.toInt
    val specialOps = lines.collect { case opPattern(op, name) => (op, name) }

    rawOps.flatMap { rawOp =>
      rawOp.funct3s.filter(_ != " ").map(funct3 => Op(
        rawOp.tpe, rawOp.funct6, funct3, rawOp.name, None,
      ))
    }.flatMap { op =>
      if (op.name == name)
        specialOps.map(sp =>
          op.copy(name = sp._2, special = Some(SpecialAux(name, vs, sp._1))))
      else
        Array(op)
    }
  }
}

object InstructionDecodeTable {
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

  def value(op: Op): Value = {
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
    val units = if (op.special.isEmpty) {
      b2s(logicUnit) + b2s(addUnit) + b2s(shiftUnit) + b2s(mulUnit) + b2s(divUnit) + b2s(otherUnit)
    } else "000001"
    val uop = if (op.special.isEmpty) {
      if (mulUnit) {
        val high = op.name.contains("mulh")
        val n = if (high) 3 else firstIndexContains(mul2, op.name)
        require(n < 4)
        b2s(op.name.startsWith("vn")) + b2s(Seq("c", "cu", "cus", "csu").exists(op.name.endsWith)) +
          ("00" + n.toBinaryString takeRight 2)
      } else if (divUnit) {
        val n = firstIndexContains(div, op.name)
        require(n < 2)
        "?" * 3 + n.toBinaryString
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
        "?" * 2 + ("00" + n.toBinaryString takeRight 2)
      } else if (otherUnit) {
        val n = firstIndexContains(other, op.name)
        require(n < 8)
        "0" + ("000" + n.toBinaryString takeRight 3)
      } else {
        // unreachable
        require(false)
        "?" * 4
      }
    } else
      "1" + (
        if (ffoUnit)
          "?" +
            ("00" + ffo.indexOf(op.name).toBinaryString takeRight 2)
        else if (op.special.get.name == "VXUNARY0") {
          val log2 = (x: Int) => (math.log10(x) / math.log10(2)).toInt
          b2s(op.name.startsWith("vs")) +
            ("00" + log2(op.name.last.toString.toInt).toBinaryString takeRight 2)
        } else
          "?" * 3
        )

    val nameWoW = op.name.replace(".w", "")
    val controls = if (op.special.isEmpty)
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
      "?" * 4 +
        b2s(op.special.get.name == "VWXUNARY0") +
        b2s(op.special.get.name == "VXUNARY0") +
        b2s(op.name.startsWith("vmv")) +
        b2s(ffoUnit) +
        b2s(op.name == "vcpop") +
        b2s(op.name == "viota") +
        b2s(op.name == "vid") +
        b2s(op.funct3 == "V")
    Value(
      units, uop, controls,
      v = op.funct3 == "V",
      x = op.funct3 == "X",
      i = if (op.special.nonEmpty) None else Some(op.funct3 == "I"),
    )
  }

  def table: List[(BitPat, BitPat)] = {
    Decoder.ops
      // TODO: floating point instructions are not supported for now.
      .filter(_.tpe != "F").map(x => x.bitPat -> BitPat("b" + value(x))).toList
  }
}

import chisel3._

trait Field {
  def width: Int
  def genTable(op: Op): BitPat
  def dc: BitPat = BitPat.dontCare(width)
}

trait BoolField extends Field {
  def width: Int = 1
  def y: BitPat = BitPat.Y(1)
  def n: BitPat = BitPat.N(1)
}

class DecodeBundle(fields: Seq[Field]) extends Record with chisel3.experimental.AutoCloneType {
  override def elements: SeqMap[String, UInt] = fields.map(k => k.toString -> UInt(k.width.W)).to(SeqMap)
  def apply(field: Field): UInt = elements(field.toString)
}

object DecodeTable {
  object logic extends BoolField {
    val subs: Seq[String] = Seq("and", "or")
    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains)) y else n
  }

  object adder extends BoolField {
    val subs: Seq[String] = Seq(
      "add", "sub", "slt", "sle", "sgt", "sge",
      "max", "min", "seq", "sne", "adc", "sbc", "sum"
    )
    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains) &&
      !(op.tpe == "M" && Seq("vm", "vnm").exists(op.name.startsWith))) y else n
  }

  object shift extends BoolField {
    val subs: Seq[String] = Seq(
      "srl", "sll", "sra"
    )
    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains)) y else n
  }

  object multiplier extends BoolField {
    val subs: Seq[String] = Seq(
      "mul", "madd", "macc", "msub", "msac"
    )
    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains)) y else n
  }

  object divider extends BoolField {
    val subs: Seq[String] = Seq(
      "div", "rem"
    )
    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains)) y else n
  }

  object other extends BoolField {
    val subs: Seq[String] = Seq(
      "slide", "rgather", "merge", "mv", "clip", "compress"
    )
    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains)) y else n
  }

  object firstWiden extends BoolField {
    def genTable(op: Op): BitPat = if (op.name.endsWith(".w")) y else n
  }

  object nr extends BoolField {
    def genTable(op: Op): BitPat = if (op.name.contains("<nr>")) y else n
  }

  object red extends BoolField {
    def genTable(op: Op): BitPat = if (op.name.contains("red")) y else n
  }

  object reverse extends BoolField {
    def genTable(op: Op): BitPat = if (op.name == "vrsub") y else n
  }

  object narrow extends BoolField {
    val subs: Seq[String] = Seq(
      "vnsrl", "vnsra", "vnclip"
    )
    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains)) y else n
  }

  object widen extends BoolField {
    def genTable(op: Op): BitPat = if (op.name.startsWith("vw")) y else n
  }

  object average extends BoolField {
    val subs: Seq[String] = Seq(
      "vaa", "vas"
    )
    def genTable(op: Op): BitPat = if (subs.exists(op.name.startsWith)) y else n
  }

  object unsigned0 extends BoolField {
    def genTable(op: Op): BitPat = if (
      op.name.endsWith("us.w") || (op.name.endsWith("u.w") && !op.name.endsWith("su.w"))
    ) y else n
  }

  object unsigned1 extends BoolField {
    def genTable(op: Op): BitPat = if (op.name.endsWith("u")) y else n
  }

  object vtype extends BoolField {
    def genTable(op: Op): BitPat = if (op.funct3 == "V") y else n
  }

  object xtype extends BoolField {
    def genTable(op: Op): BitPat = if (op.funct3 == "X") y else n
  }

  object targetRd extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.nonEmpty && op.special.get.name == "VWXUNARY0") y else n
  }

  object extend extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.nonEmpty && op.special.get.name == "VXUNARY0") y else n
  }

  object mv extends BoolField {
    def genTable(op: Op): BitPat = if (op.name.startsWith("vmv")) y else n
  }

  object ffo extends BoolField {
    val subs: Seq[String] = Seq(
      "vfirst", "vmsbf", "vmsof", "vmsif"
    )

    def genTable(op: Op): BitPat = if (subs.exists(op.name.contains)) y else n
  }

  object popCount extends BoolField {
    def genTable(op: Op): BitPat = if (op.name == "vcpop") y else n
  }

  object iota extends BoolField {
    def genTable(op: Op): BitPat = if (op.name == "viota") y else n
  }

  object id extends BoolField {
    def genTable(op: Op): BitPat = if (op.name == "vid") y else n
  }

  val all: Seq[Field] = Seq(
    logic, adder, shift, multiplier, divider, other,
    firstWiden, nr, red, reverse, narrow, widen, average, unsigned0, unsigned1,
    vtype, xtype,
    // TODO: uop
    targetRd, extend, mv, ffo, popCount, iota, id
    // TODO: specialUop
  )

  def bundle = new DecodeBundle(all)

  def table: TruthTable = {
    val result = Decoder.ops.map { op =>
      op.bitPat -> all.map(_.genTable(op)).reduce(_ ## _)
    }.to(SeqMap)
    TruthTable(result, BitPat.dontCare(result.head._2.getWidth))
  }
}
