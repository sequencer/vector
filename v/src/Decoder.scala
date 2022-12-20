package v

import io.Source
import collection._
import scala.annotation.unused

object Implicits {
  implicit class a2i(val digits: String) {
    def base(b: Int): Int = Integer.parseInt(digits, b)
    def b: Int = base(2)
    def d: Int = base(10)
  }
}
import Implicits._

object Funct3s extends Enumeration {
  type Funct3 = Value

  @unused val OPIVV: Value = Value("000".b)
  @unused val OPIVX: Value = Value("100".b)
  @unused val OPIVI: Value = Value("011".b)
  @unused val OPMVV: Value = Value("010".b)
  @unused val OPMVX: Value = Value("110".b)
  @unused val OPFVV: Value = Value("001".b)
  @unused val OPFVF: Value = Value("101".b)

  def str(v: Funct3): String = "000" + v.id.toBinaryString takeRight 3
  def suffix(v: Funct3): String = v.toString.last.toLower.toString
}
import Funct3s._

object Ops extends Enumeration {
  type Op = Value
  @unused val OPIV, OPMV, OPFV = Value
}
import Ops._

object Specials extends Enumeration {
  type Special = Value
  type SpecialsMap = Map[Special, mutable.ArrayBuffer[(Int, String, String)]]
  @unused val VRXUNARY0, VWXUNARY0, VXUNARY0, VRFUNARY0,
      VWFUNARY0, VFUNARY0, VFUNARY1, VMUNARY0 = Value
}
import Specials._

case class InsnGroup(name: String,
                     funct3s: Seq[Funct3], funct6: String,
                     op: Op, special: Option[(Special, Int, String)]) {
  require(funct6.length == 6)

  def fullName(funct3: Funct3): String = {
    if (special.nonEmpty) name else s"$name.${Funct3s.suffix(funct3)}"
  }

  def expandSpecial(specials: SpecialsMap): Seq[InsnGroup] = {
    if (name.startsWith("v")) return Seq(this)

    val s = Specials.withName(this.name)
    specials(s).map(x => InsnGroup(
      name = x._3,
      funct3s = funct3s,
      funct6 = funct6,
      op = op,
      special = Some((s, x._1, x._2))
    )).toSeq
  }

  def keys(): Seq[String] = {
    funct3s.
      map(x =>
        funct6 +                                             // funct6
        "?" +                                                // always '?', but why?
        (if (special.isEmpty || special.get._2 == 1) "?????"
        else special.get._3) +                               // vs2
        (if (special.isEmpty || special.get._2 == 2) "?????"
        else special.get._3) +                               // vs1
        Funct3s.str(x)                                       // funct3
      )
  }

  def values(): Seq[(String, String)] = {
    funct3s.map(x => (aux(x).toString, fullName(x)))
  }

  def mapping(): Map[String, (String, String)] = {
    SortedMap[String, (String, String)](keys().zip(values()).toSeq: _*)
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

  case class Aux(units: String, uop: String, controls: String,
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

  def aux(funct3: Funct3): Aux = {
    val b2s = (b: Boolean) => if (b) "1" else "0"
    val firstIndexContains = (xs: Iterable[String], s: String) =>
      xs.map(s.indexOf).zipWithIndex.filter(_._1 != -1).head._2

    val logicUnit = logic.exists(name.contains)
    val addUnit = add.exists(name.contains) && !(op == OPMV && Seq("vm", "vnm").exists(name.startsWith))
    val shiftUnit = shift.exists(name.contains)
    val mulUnit = mul.exists(name.contains)
    val divUnit = div.exists(name.contains)
    val otherUnit = other.exists(name.contains)
    val ffoUnit = ffo.contains(name)
    val units = if (special.isEmpty) {
      b2s(logicUnit) + b2s(addUnit) + b2s(shiftUnit) + b2s(mulUnit) + b2s(divUnit) + b2s(otherUnit)
    } else "000001"
    val uop = if (special.isEmpty) {
      if (mulUnit){
        val high = name.contains("mulh")
        val n = if (high) 3 else firstIndexContains(mul2, name)
        require(n < 4)
        b2s(name.startsWith("vn")) + b2s(Seq("c", "cu", "cus", "csu").exists(name.endsWith)) +
        ("00" + n.toBinaryString takeRight 2)
      } else if (divUnit) {
        val n = firstIndexContains(div, name)
        require(n < 2)
        "?"*3 + n.toBinaryString
      } else if (addUnit) {
        val n = if (name.contains("sum")) 0 else firstIndexContains(add, name)
        require(n < 16)
        ("0000" + n.toBinaryString takeRight 4)
      } else if (logicUnit) {
        val isXnor = name == "vmxnor"
        val isXor = name.contains("xor")
        val n = if (isXnor || isXor) 2 else firstIndexContains(logic, name)
        require(n < 4)
        b2s(name.startsWith("vmn")) +
        b2s(isXnor || name.contains("not")) +
        ("00" + n.toBinaryString takeRight 2)
      } else if (shiftUnit) {
        val n = firstIndexContains(shift, name)
        require(n < 4)
        "?"*2 + ("00" + n.toBinaryString takeRight 2)
      } else if (otherUnit) {
        val n =firstIndexContains(other, name)
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
          ("00" + ffo.indexOf(name).toBinaryString takeRight 2)
        else if (special.get._1 == VXUNARY0) {
          val log2 = (x: Int) => (math.log10(x)/math.log10(2)).toInt
          b2s(name.startsWith("vs")) +
          ("00" + log2(name.last.toString.d).toBinaryString takeRight 2)
        } else
          "?"*3
      )

    val nameWithoutW = name.replace(".w", "")
    val controls = if (special.isEmpty)
      b2s(name.endsWith(".w")) +
      b2s(name.contains("ei16")) +
      b2s(name.contains("<nr>")) +
      b2s(name.contains("red")) +
      b2s(name.startsWith("vm") && ((addUnit && !Seq("min", "max").exists(name.contains)) || logicUnit)) +
      b2s(name == "vrsub") +
      b2s(name.startsWith("vn") && (shiftUnit || otherUnit)) +
      b2s(name.startsWith("vw")) +
      b2s(Seq("vsa", "vss", "vsm").exists(name.startsWith)) +
      b2s(Seq("vaa", "vas").exists(name.startsWith)) +
      b2s(nameWithoutW.endsWith("us") || (nameWithoutW.endsWith("u") && !nameWithoutW.endsWith("su"))) +
      b2s(nameWithoutW.endsWith("u"))
    else
      "?"*4 +
      b2s(special.get._1 == VWXUNARY0) +
      b2s(special.get._1 == VXUNARY0) +
      b2s(name.startsWith("vmv")) +
      b2s(ffoUnit) +
      b2s(name == "vcpop") +
      b2s(name == "viota") +
      b2s(name == "vid") +
      b2s(funct3.toString.endsWith("V"))
    Aux(
      units, uop, controls,
      v = funct3.toString.endsWith("V"),
      x = funct3.toString.endsWith("X"),
      i = if (special.nonEmpty) None else Some(funct3.toString.endsWith("I")),
    )
  }
}

object InsnGroup {
  def parsePart(part: Seq[String], idx: Int): Option[InsnGroup] = {
    require(3 <= part.length && part.length <= 5)
    require(0 <= idx && idx <= 2)

    if (part.length == 3 || part.last.isEmpty) {
      return None
    }

    val op = Ops(idx)
    val funct3s = part.slice(1, part.length-1).
      filter(_.nonEmpty).
      map(x => Funct3s.withName(op.toString + x))

    Some(InsnGroup(
      name = part.last,
      funct3s = funct3s,
      funct6 = part.head,
      op = op,
      special = None,
    ))
  }

  def parseLine(specials: SpecialsMap)
               (line: String): Seq[InsnGroup] = {
    val (x, xs) = line.split('|').drop(1).map(_.trim).splitAt(5)
    val parts = xs.grouped(4).toArray.+:(x)
    parts.zipWithIndex.flatMap(x => parsePart(
      x._1, x._2)).flatMap(_.expandSpecial(specials))
  }
}

object Main {
  def lines(filename: String): Seq[String] = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toArray
    source.close()
    lines
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 2)

    val specials = mutable.Map[Special, mutable.ArrayBuffer[(Int, String, String)]]()
    var special: Option[Special] = None
    var operand: Option[Int] = None
    for (line <- lines(args(1)).filter(_.nonEmpty)) {
      if (line.startsWith("V"))
        special = Some(Specials.withName(line))
      else if (line.startsWith("vs"))
        operand = Some(s"${line(2)}".d)
      else {
        val parts = line.split('|').drop(1).map(_.trim)
        require(parts.length == 2)

        val v = specials.getOrElse(special.get, mutable.ArrayBuffer.empty)
        v += ((operand.get, parts.head, parts.last))
        specials += ((special.get, v))
      }
    }

    val groups = lines(args(0)).flatMap(InsnGroup.parseLine(specials))

    // TODO: floating point instructions are not supported for now.
    groups.filter(_.op != OPFV).flatMap(_.mapping()).foreach(x => {
      println(s"${" "*4}BitPat(\"b${x._1}\") -> BitPat(\"b${x._2._1}\"), // ${x._2._2}")
    })
  }
}
