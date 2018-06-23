package de.thm.puck.assembler

import AbstractSyntax._

object AbstractSyntaxPrinter {
  private def formatValue(value: Long): String = value.toString//if (value == 0) "0" else if (value < 0) "-0x" + (-value).toHexString.toUpperCase else "0x" + value.toHexString.toUpperCase

  private def formatRegister(reg: Int): String = "$" + reg

  def apply(obj: AbstractSyntax.Object): String = {
    obj.map {
      case Label(label) => label + ":"
      case Comment(com) => "; " + com

      case ExportDirective(label) => ".export " + label.identifier
      case ImportDirective(from, name, internalName) => ".import " + from + " " + name + " " + internalName
      case ObjectDirective(name) => ".object " + name
      case InitializationDirective(label: Tokens.Identifier) => ".initialization " + label.identifier
      case ExecutableDirective(label: Label) => ".executable " + label.name
      case ByteDirective(data) => ".byte " + data.map(formatValue).getOrElse("")
      case HalfwordDirective(data) => ".halfword " + data.map(formatValue).getOrElse("")
      case WordDirective(data) => ".word " + data.map(formatValue).getOrElse("")

      case Add(dstR: Int, op1R: Int, op2R: Int) => "ADD " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Sub(dstR: Int, op1R: Int, op2R: Int) => "SUB " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Mulu(dstR: Int, op1R: Int, op2R: Int) => "MULU " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Divu(dstR: Int, op1R: Int, op2R: Int) => "DIVU " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Modu(dstR: Int, op1R: Int, op2R: Int) => "MODU " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Muli(dstR: Int, op1R: Int, op2R: Int) => "MULI " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Divi(dstR: Int, op1R: Int, op2R: Int) => "DIVI " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Modi(dstR: Int, op1R: Int, op2R: Int) => "MODI " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case And(dstR: Int, op1R: Int, op2R: Int) => "AND " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Or(dstR: Int, op1R: Int, op2R: Int) => "OR " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Xor(dstR: Int, op1R: Int, op2R: Int) => "XOR " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Sl(dstR: Int, op1R: Int, op2R: Int) => "SL " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Sr(dstR: Int, op1R: Int, op2R: Int) => "SR " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Eq(dstR: Int, op1R: Int, op2R: Int) => "EQ " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Ne(dstR: Int, op1R: Int, op2R: Int) => "NE " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Lti(dstR: Int, op1R: Int, op2R: Int) => "LTI " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Lei(dstR: Int, op1R: Int, op2R: Int) => "LEI " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Gti(dstR: Int, op1R: Int, op2R: Int) => "GTI " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Gei(dstR: Int, op1R: Int, op2R: Int) => "GEI " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Ltu(dstR: Int, op1R: Int, op2R: Int) => "LTU " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Leu(dstR: Int, op1R: Int, op2R: Int) => "LEU " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Gtu(dstR: Int, op1R: Int, op2R: Int) => "GTU " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Geu(dstR: Int, op1R: Int, op2R: Int) => "GEU " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Brt(srcR: Int, label: Label) => "BRT " + formatRegister(srcR) + " " + label.name
      case Brf(srcR: Int, label: Label) => "BRF " + formatRegister(srcR) + " " + label.name
      case Jmp(label: Label) => "JMP " + label.name
      case Jmpr(dstR: Int) => "JMPR " + formatRegister(dstR)
      case Call(retR: Int, label: Label) => "CALL " + formatRegister(retR) + " " + label.name
      case Callr(retR: Int, dstR: Int) => "CALLR " + formatRegister(retR) + " " + formatRegister(dstR)
      case Halt => "HALT"
      case Ldb(srcR: Int, addressR: Int) => "LDB " + formatRegister(srcR) + " " + formatRegister(addressR)
      case Ldhw(srcR: Int, addressR: Int) => "LDHW " + formatRegister(srcR) + " " + formatRegister(addressR)
      case Ldw(srcR: Int, addressR: Int) => "LDW " + formatRegister(srcR) + " " + formatRegister(addressR)
      case Stb(dstR: Int, addressR: Int) => "STB " + formatRegister(dstR) + " " + formatRegister(addressR)
      case Sthw(dstR: Int, addressR: Int) => "STHW " + formatRegister(dstR) + " " + formatRegister(addressR)
      case Stw(dstR: Int, addressR: Int) => "STW " + formatRegister(dstR) + " " + formatRegister(addressR)
      case Inc(dstR: Int) => "INC " + formatRegister(dstR)
      case Inu(dstR: Int) => "INU " + formatRegister(dstR)
      case Ini(dstR: Int) => "INI " + formatRegister(dstR)
      case Inf(dstR: Int) => "INF " + formatRegister(dstR)
      case Outc(srcR: Int) => "OUTC " + formatRegister(srcR)
      case Outu(srcR: Int) => "OUTU " + formatRegister(srcR)
      case Outi(srcR: Int) => "OUTI " + formatRegister(srcR)
      case Outf(srcR: Int) => "OUTF " + formatRegister(srcR)
      case Setb(dstR: Int, immediate: Long) => "SETB " + formatRegister(dstR) + " " + formatValue(immediate)
      case Sethw(dstR: Int, immediate: Long) => "SETHW " + formatRegister(dstR) + " " + formatValue(immediate)
      case Setw(dstR: Int, value: Either[Long, Tokens.Identifier]) => "SETW " + formatRegister(dstR) + " " + (if (value.isLeft) formatValue(value.left.get) else value.right.get.identifier)
      case Cp(dst: Int, src: Int) => "CP " + formatRegister(dst) + " " + formatRegister(src)
      case Addf(dstR: Int, op1R: Int, op2R: Int) => "ADDF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Subf(dstR: Int, op1R: Int, op2R: Int) => "SUBF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Mulf(dstR: Int, op1R: Int, op2R: Int) => "MULF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Divf(dstR: Int, op1R: Int, op2R: Int) => "DIVF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Ltf(dstR: Int, op1R: Int, op2R: Int) => "LTF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Lef(dstR: Int, op1R: Int, op2R: Int) => "LEF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Gtf(dstR: Int, op1R: Int, op2R: Int) => "GTF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Gef(dstR: Int, op1R: Int, op2R: Int) => "GEF " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatRegister(op2R)
      case Addc(dstR: Int, op1R: Int, op2: Long) => "ADDC " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatValue(op2)
      case Subc(dstR: Int, op1R: Int, op2: Long) => "SUBC " + formatRegister(dstR) + " " + formatRegister(op1R) + " " + formatValue(op2)
      case NoOp => "ERROR"
    }.map(_ + "\n").fold("")((a, b) => a + b)
  }
}
