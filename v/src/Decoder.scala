package v

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode._

trait FieldName {
  def name: String = this.getClass.getSimpleName.replace("$", "")
}

trait UopField extends DecodeField[Op, UInt] with FieldName {
  def chiselType: UInt = UInt(4.W)
}

trait BoolField extends BoolDecodeField[Op] with FieldName

object Decoder {
  object logic extends BoolField {
    val subs: Seq[String] = Seq("and", "or")
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (subs.exists(op.name.contains)) y else n
  }

  object adder extends BoolField {
    val subs: Seq[String] = Seq(
      "add",
      "sub",
      "slt",
      "sle",
      "sgt",
      "sge",
      "max",
      "min",
      "seq",
      "sne",
      "adc",
      "sbc",
      "sum"
    )
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc
    else if (
      subs.exists(op.name.contains) &&
        !(op.tpe == "M" && Seq("vm", "vnm").exists(op.name.startsWith))
    ) y
    else n
  }

  object shift extends BoolField {
    val subs: Seq[String] = Seq(
      "srl",
      "sll",
      "sra"
    )
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (subs.exists(op.name.contains)) y else n
  }

  object multiplier extends BoolField {
    val subs: Seq[String] = Seq(
      "mul",
      "madd",
      "macc",
      "msub",
      "msac"
    )
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (subs.exists(op.name.contains)) y else n
  }

  object divider extends BoolField {
    val subs: Seq[String] = Seq(
      "div",
      "rem"
    )
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (subs.exists(op.name.contains)) y else n
  }

  object other extends BoolField {
    val subs: Seq[String] = Seq(
      "slide",
      "rgather",
      "merge",
      "mv",
      "clip",
      "compress"
    )
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) y else if (subs.exists(op.name.contains)) y else n
  }

  object firstWiden extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (op.name.endsWith(".w")) y else n
  }

  object nr extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (op.name.contains("<nr>")) y else n
  }

  object red extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (op.name.contains("red")) y else n
  }

  object reverse extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (op.name == "vrsub") y else n
  }

  object narrow extends BoolField {
    val subs: Seq[String] = Seq(
      "vnsrl",
      "vnsra",
      "vnclip"
    )
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (subs.exists(op.name.contains)) y else n
  }

  object widen extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (op.name.startsWith("vw")) y else n
  }

  object average extends BoolField {
    val subs: Seq[String] = Seq(
      "vaa",
      "vas"
    )
    def genTable(op: Op): BitPat = if (op.special.nonEmpty) dc else if (subs.exists(op.name.startsWith)) y else n
  }

  object unsigned0 extends BoolField {
    def genTable(op: Op): BitPat = {
      val nameWoW = op.name.replace(".w", "")
      if (op.special.nonEmpty) dc
      else if (nameWoW.endsWith("us") || (nameWoW.endsWith("u") && !nameWoW.endsWith("su"))) y
      else n
    }
  }

  object unsigned1 extends BoolField {
    def genTable(op: Op): BitPat = {
      val nameWoW = op.name.replace(".w", "")
      if (op.special.nonEmpty) dc else if (nameWoW.endsWith("u")) y else n
    }
  }

  object vtype extends BoolField {
    def genTable(op: Op): BitPat = if (op.funct3 == "V") y else n
  }

  object xtype extends BoolField {
    def genTable(op: Op): BitPat = if (op.funct3 == "X") y else n
  }

  object targetRd extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.isEmpty) dc else if (op.special.get.name == "VWXUNARY0") y else n
  }

  object extend extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.isEmpty) dc else if (op.special.get.name == "VXUNARY0") y else n
  }

  object mv extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.isEmpty) dc else if (op.name.startsWith("vmv")) y else n
  }

  object ffo extends BoolField {
    val subs: Seq[String] = Seq(
      "vfirst",
      "vmsbf",
      "vmsof",
      "vmsif"
    )

    def genTable(op: Op): BitPat = if (op.special.isEmpty) dc else if (subs.exists(op.name.contains)) y else n
  }

  object popCount extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.isEmpty) dc else if (op.name == "vcpop") y else n
  }

  object iota extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.isEmpty) dc else if (op.name == "viota") y else n
  }

  object id extends BoolField {
    def genTable(op: Op): BitPat = if (op.special.isEmpty) dc else if (op.name == "vid") y else n
  }

  // TODO[2]: uop should be well documented
  object uop extends UopField {
    val mul: Seq[String] = Seq(
      "mul",
      "ma",
      "ms"
    )

    def y: BitPat = BitPat.Y(1)
    def genTable(op: Op): BitPat = {
      val b2s = (b: Boolean) => if (b) "1" else "0"
      val firstIndexContains = (xs: Iterable[String], s: String) =>
        xs.map(s.indexOf).zipWithIndex.filter(_._1 != -1).head._2

      val table = if (op.special.nonEmpty) {
        "????"
      } else if (multiplier.genTable(op) == y) {
        val high = op.name.contains("mulh")
        val n = if (high) 3 else firstIndexContains(mul, op.name)
        require(n < 4)
        b2s(op.name.startsWith("vn")) + b2s(Seq("c", "cu", "cus", "csu").exists(op.name.endsWith)) +
          (("00" + n.toBinaryString).takeRight(2))
      } else if (divider.genTable(op) == y) {
        val n = firstIndexContains(divider.subs, op.name)
        require(n < 2)
        "?" * 3 + n.toBinaryString
      } else if (adder.genTable(op) == y) {
        val n = if (op.name.contains("sum")) 0 else firstIndexContains(adder.subs, op.name)
        require(n < 16)
        (("0000" + n.toBinaryString).takeRight(4))
      } else if (logic.genTable(op) == y) {
        val isXnor = op.name == "vmxnor"
        val isXor = op.name.contains("xor")
        val n = if (isXnor || isXor) 2 else firstIndexContains(logic.subs, op.name)
        require(n < 4)
        b2s(op.name.startsWith("vmn")) +
          b2s(isXnor || op.name.contains("not")) +
          (("00" + n.toBinaryString).takeRight(2))
      } else if (shift.genTable(op) == y) {
        val n = firstIndexContains(shift.subs, op.name)
        require(n < 4)
        "?" * 2 + (("00" + n.toBinaryString).takeRight(2))
      } else if (other.genTable(op) == y) {
        val n = firstIndexContains(other.subs, op.name)
        require(n < 8)
        "0" + (("000" + n.toBinaryString).takeRight(3))
      } else {
        // unreachable
        require(false)
        "?" * 4
      }

      BitPat("b" + table)
    }
  }

  object specialUop extends UopField {
    def y: BitPat = BitPat.Y(1)
    def genTable(op: Op): BitPat = {
      val b2s = (b: Boolean) => if (b) "1" else "0"
      val table =
        if (op.special.isEmpty) "????"
        else
          "1" + (
            if (ffo.genTable(op) == y)
              "?" +
                (("00" + ffo.subs.indexOf(op.name).toBinaryString).takeRight(2))
            else if (op.special.get.name == "VXUNARY0") {
              val log2 = (x: Int) => (math.log10(x) / math.log10(2)).toInt
              b2s(op.name.startsWith("vs")) +
                (("00" + log2(op.name.last.toString.toInt).toBinaryString).takeRight(2))
            } else
              "?" * 3
            )
      BitPat("b" + table)
    }
  }

  val all: Seq[DecodeField[Op, _ >: Bool <: UInt]] = Seq(
    logic,
    adder,
    shift,
    multiplier,
    divider,
    other,
    firstWiden,
    nr,
    red,
    reverse,
    narrow,
    widen,
    average,
    unsigned0,
    unsigned1,
    vtype,
    xtype,
    uop,
    targetRd,
    extend,
    mv,
    ffo,
    popCount,
    iota,
    id,
    specialUop
  )

  private val decodeTable: DecodeTable[Op] = new DecodeTable[Op](SpecInstTableParser.ops, all)
  def decode: UInt => DecodeBundle = decodeTable.decode
  def bundle: DecodeBundle = decodeTable.bundle
}
