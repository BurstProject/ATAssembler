import java.util.ArrayList
import java.nio.ByteBuffer
import java.io.ByteArrayOutputStream
import java.nio.ByteOrder
import scala.collection.mutable.HashMap

abstract sealed class LocFill
case class LabelByteFill(loc: Int, label: String, offset: Int) extends LocFill
case class LabelIntFill(loc: Int, label: String) extends LocFill
case class VariableFill(loc: Int, name: String) extends LocFill

object ATAssembler {
  
  val whitespace = "^\\s*$".r
  val label = "(\\w+):".r
  val comment = "\\^comment.*".r
  val declare = "\\^declare\\s+(\\w+)".r
  val allocate = "\\^allocate\\s+(\\w+)\\s+(\\d+)".r
  val set_val = "SET\\s+@(\\w+)\\s+#([\\da-f]+)".r
  val set_dat = "SET\\s+@(\\w+)\\s+\\$(\\w+)".r
  val clr_dat = "CLR\\s+@(\\w+)".r
  val inc_dat = "INC\\s+@(\\w+)".r
  val dec_dat = "DEC\\s+@(\\w+)".r
  val add_dat = "ADD\\s+@(\\w+)\\s+\\$(\\w+)".r
  val sub_dat = "SUB\\s+@(\\w+)\\s+\\$(\\w+)".r
  val mul_dat = "MUL\\s+@(\\w+)\\s+\\$(\\w+)".r
  val div_dat = "DIV\\s+@(\\w+)\\s+\\$(\\w+)".r
  val bor_dat = "BOR\\s+@(\\w+)\\s+\\$(\\w+)".r
  val and_dat = "AND\\s+@(\\w+)\\s+\\$(\\w+)".r
  val xor_dat = "XOR\\s+@(\\w+)\\s+\\$(\\w+)".r
  val not_dat = "NOT\\s+@(\\w+)".r
  val set_ind = "SET\\s+@(\\w+)\\s+\\$\\(\\$(\\w+)\\)".r
  val set_idx = "SET\\s+@(\\w+)\\s+\\$\\(\\$(\\w+)\\s*\\+\\s*\\$(\\w+)\\)".r
  val psh_dat = "PSH\\s+\\$(\\w+)".r
  val pop_dat = "POP\\s+@(\\w+)".r
  val jmp_sub = "JSR\\s+:(\\w+)".r
  val ret_sub = "RET".r
  val ind_dat = "SET\\s+@\\(\\$(\\w+)\\)\\s+\\$(\\w+)".r
  val idx_dat = "SET\\s+@\\(\\$(\\w+)\\s*\\+\\s*\\$(\\w+)\\)\\s+\\$(\\w+)".r
  val mod_dat = "MOD\\s+@(\\w+)\\s+\\$(\\w+)".r
  val shl_dat = "SHL\\s+@(\\w+)\\s+\\$(\\w+)".r
  val shr_dat = "SHR\\s+@(\\w+)\\s+\\$(\\w+)".r
  val jmp_adr = "JMP\\s+:(\\w+)".r
  val bzr_dat = "BZR\\s+\\$(\\w+)\\s+:(\\w+)".r
  val bnz_dat = "BNZ\\s+\\$(\\w+)\\s+:(\\w+)".r
  val bgt_dat = "BGT\\s+\\$(\\w+)\\s+\\$(\\w+)\\s+:(\\w+)".r
  val blt_dat = "BLT\\s+\\$(\\w+)\\s+\\$(\\w+)\\s+:(\\w+)".r
  val bge_dat = "BGE\\s+\\$(\\w+)\\s+\\$(\\w+)\\s+:(\\w+)".r
  val ble_dat = "BLE\\s+\\$(\\w+)\\s+\\$(\\w+)\\s+:(\\w+)".r
  val beq_dat = "BEQ\\s+\\$(\\w+)\\s+\\$(\\w+)\\s+:(\\w+)".r
  val bne_dat = "BNE\\s+\\$(\\w+)\\s+\\$(\\w+)\\s+:(\\w+)".r
  val fiz_dat = "FIZ\\s+\\$(\\w+)".r
  val slp_dat = "SLP\\s+\\$(\\w+)".r
  val stz_dat = "STZ\\s+\\$(\\w+)".r
  val fin_imd = "FIN".r
  val stp_imd = "STP".r
  val err_adr = "ERR\\s+:(\\w+)".r
  val set_pcs = "PCS".r
  val ext_fun = "FUN\\s+(\\w+)".r
  val ext_fun_dat = "FUN\\s+(\\w+)\\s+\\$(\\w+)".r
  val ext_fun_dat_2 = "FUN\\s+(\\w+)\\s+\\$(\\w+)\\s+\\$(\\w+)".r
  val ext_fun_ret = "FUN\\s+@(\\w+)\\s+(\\w+)".r
  val ext_fun_ret_dat = "FUN\\s+@(\\w+)\\s+(\\w+)\\s+\\$(\\w+)".r
  val ext_fun_ret_dat_2 = "FUN\\s+@(\\w+)\\s+(\\w+)\\s+\\$(\\w+)\\s+\\$(\\w+)".r
  val nop = "NOP".r
  
  val functions = new HashMap[String, Short]
  functions += "get_A1" -> 0x0100
  functions += "get_A2" -> 0x0101
  functions += "get_A3" -> 0x0102
  functions += "get_A4" -> 0x0103
  functions += "get_B1" -> 0x0104
  functions += "get_B2" -> 0x0105
  functions += "get_B3" -> 0x0106
  functions += "get_B4" -> 0x0107
  functions += "set_A1" -> 0x0110
  functions += "set_A2" -> 0x0111
  functions += "set_A3" -> 0x0112
  functions += "set_A4" -> 0x0113
  functions += "set_A1_A2" -> 0x0114
  functions += "set_A3_A4" -> 0x0115
  functions += "set_B1" -> 0x0116
  functions += "set_B2" -> 0x0117
  functions += "set_B3" -> 0x0118
  functions += "set_B4" -> 0x0119
  functions += "set_B1_B2" -> 0x011a
  functions += "set_B3_B4" -> 0x011b
  functions += "clear_A" -> 0x0120
  functions += "clear_B" -> 0x0121
  functions += "clear_A_B" -> 0x0122
  functions += "copy_A_From_B" -> 0x0123
  functions += "copy_B_From_A" -> 0x0124
  functions += "check_A_Is_Zero" -> 0x0125
  functions += "check_B_Is_Zero" -> 0x0126
  functions += "check_A_equals_B" -> 0x0127
  functions += "swap_A_and_B" -> 0x0128
  functions += "OR_A_with_B" -> 0x0129
  functions += "OR_B_with_A" -> 0x012a
  functions += "AND_A_with_B" -> 0x012b
  functions += "AND_B_with_A" -> 0x012c
  functions += "XOR_A_with_B" -> 0x012d
  functions += "XOR_B_with_A" -> 0x012e
  functions += "add_A_to_B" -> 0x0140
  functions += "add_B_to_A" -> 0x0141
  functions += "sub_A_from_B" -> 0x0142
  functions += "sub_B_from_A" -> 0x0143
  functions += "mul_A_by_B" -> 0x0144
  functions += "mul_B_by_A" -> 0x0145
  functions += "div_A_by_B" -> 0x0146
  functions += "div_B_by_A" -> 0x0147
  functions += "MD5_A_to_B" -> 0x0200
  functions += "check_MD5_A_with_B" -> 0x0201
  functions += "HASH160_A_to_B" -> 0x0202
  functions += "check_HASH160_A_with_B" -> 0x0203
  functions += "SHA256_A_to_B" -> 0x0204
  functions += "check_SHA256_A_with_B" -> 0x0205
  functions += "get_Block_Timestamp" -> 0x0300
  functions += "get_Creation_Timestamp" -> 0x0301
  functions += "get_Last_Block_Timestamp" -> 0x0302
  functions += "put_Last_Block_Hash_In_A" -> 0x0303
  functions += "A_to_Tx_after_Timestamp" -> 0x0304
  functions += "get_Type_for_Tx_in_A" -> 0x0305
  functions += "get_Amount_for_Tx_in_A" -> 0x0306
  functions += "get_Timestamp_for_Tx_in_A" -> 0x0307
  functions += "get_Ticket_Id_for_Tx_in_A" -> 0x0308
  functions += "message_from_Tx_in_A_to_B" -> 0x0309
  functions += "B_to_Address_of_Tx_in_A" -> 0x030a
  functions += "B_to_Address_of_Creator" -> 0x030b
  functions += "get_Current_Balance" -> 0x0400
  functions += "get_Previous_Balance" -> 0x0401
  functions += "send_to_Address_in_B" -> 0x0402
  functions += "send_All_to_Address_in_B" -> 0x0403
  functions += "send_Old_to_Address_in_B" -> 0x0404
  functions += "send_A_to_Address_in_B" -> 0x0405
  functions += "add_Minutes_to_Timestamp" -> 0x0406

  def main(args: Array[String]): Unit = {
    
    if(args.size < 1) {
      System.out.println("requires input file")
      return
    }
    
    val labels = new HashMap[String, Int]
    val variables = new HashMap[String, Int]
    var variableCount = 0
    
    val fills = new ArrayList[LocFill]
    
    val outBytes = ByteBuffer.allocate(1000000)
    outBytes.order(ByteOrder.LITTLE_ENDIAN)
    
    val lines = io.Source.fromFile(args(0)).getLines
    for (line <- lines) {
      line match {
        case label(name) => {
          if(labels.contains(name)) {
            println("Duplicate label: " + name)
          }
          labels.put(name, outBytes.position())
        }
        case comment() =>
        case declare(name) => {
          variables += name -> variableCount;
          variableCount += 1;
        }
        case allocate(name, size) => {
          variables += name -> variableCount;
          outBytes.put(0x01.asInstanceOf[Byte])
          outBytes.putInt(variableCount)
          outBytes.putLong(variableCount + 1)
          variableCount += size.toInt + 1;
        }
        case set_val(dst, src) => {
          outBytes.put(0x01.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          outBytes.putLong(valToLong(src))
        }
        case set_dat(dst, src) => {
          outBytes.put(0x02.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case clr_dat(loc) => {
          outBytes.put(0x03.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case inc_dat(loc) => {
          outBytes.put(0x04.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case dec_dat(loc) => {
          outBytes.put(0x05.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case add_dat(dst, src) => {
          outBytes.put(0x06.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case sub_dat(dst, src) => {
          outBytes.put(0x07.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case mul_dat(dst, src) => {
          outBytes.put(0x08.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case div_dat(dst, src) => {
          outBytes.put(0x09.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case bor_dat(dst, src) => {
          outBytes.put(0x0A.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case and_dat(dst, src) => {
          outBytes.put(0x0B.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case xor_dat(dst, src) => {
          outBytes.put(0x0C.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case not_dat(loc) => {
          outBytes.put(0x0D.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case set_ind(dst, src) => {
          outBytes.put(0x0E.asInstanceOf[Byte]) //check
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case set_idx(dst, srcBase, srcOffset) => {
          outBytes.put(0x0F.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, srcBase))
          variables.getOrElseUpdate(srcBase, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, srcOffset))
          variables.getOrElseUpdate(srcOffset, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case psh_dat(loc) => {
          outBytes.put(0x10.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case pop_dat(loc) => {
          outBytes.put(0x11.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case jmp_sub(loc) => {
          outBytes.put(0x12.asInstanceOf[Byte])
          fills.add(new LabelIntFill(outBytes.position, loc))
          outBytes.putInt(0)
        }
        case ret_sub() => {
          outBytes.put(0x13.asInstanceOf[Byte]);
        }
        case ind_dat(dst, src) => {
          outBytes.put(0x14.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case idx_dat(dstBase, dstOffset, src) => {
          outBytes.put(0x15.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dstBase))
          variables.getOrElseUpdate(dstBase, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, dstOffset))
          variables.getOrElseUpdate(dstOffset, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case mod_dat(dst, src) => {
          outBytes.put(0x16.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case shl_dat(dst, src) => {
          outBytes.put(0x17.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case shr_dat(dst, src) => {
          outBytes.put(0x18.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, src))
          variables.getOrElseUpdate(src, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case jmp_adr(loc) => {
          outBytes.put(0x1a.asInstanceOf[Byte])
          fills.add(new LabelIntFill(outBytes.position, loc))
          outBytes.putInt(0)
        }
        case bzr_dat(loc, dst) => {
          outBytes.put(0x1b.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -5))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case bnz_dat(loc, dst) => {
          outBytes.put(0x1e.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -5))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case bgt_dat(arg1, arg2, dst) => {
          outBytes.put(0x1f.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -9))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case blt_dat(arg1, arg2, dst) => {
          outBytes.put(0x20.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -9))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case bge_dat(arg1, arg2, dst) => {
          outBytes.put(0x21.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -9))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case ble_dat(arg1, arg2, dst) => {
          outBytes.put(0x22.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -9))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case beq_dat(arg1, arg2, dst) => {
          outBytes.put(0x23.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -9))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case bne_dat(arg1, arg2, dst) => {
          outBytes.put(0x24.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new LabelByteFill(outBytes.position, dst, -9))
          outBytes.put(0.asInstanceOf[Byte])
        }
        case slp_dat(loc) => {
          outBytes.put(0x25.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case fiz_dat(loc) => {
          outBytes.put(0x26.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case stz_dat(loc) => {
          outBytes.put(0x27.asInstanceOf[Byte])
          fills.add(new VariableFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case fin_imd() => {
          outBytes.put(0x28.asInstanceOf[Byte])
        }
        case stp_imd() => {
          outBytes.put(0x29.asInstanceOf[Byte])
        }
        case err_adr(loc) => {
          outBytes.put(0x2B.asInstanceOf[Byte])
          fills.add(new LabelIntFill(outBytes.position, loc))
          variables.getOrElseUpdate(loc, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case set_pcs() => {
          outBytes.put(0x30.asInstanceOf[Byte])
        }
        case ext_fun(fun) => {
          outBytes.put(0x32.asInstanceOf[Byte])
          outBytes.putShort(functions(fun))
        }
        case ext_fun_dat(fun, arg) => {
          outBytes.put(0x33.asInstanceOf[Byte])
          outBytes.putShort(functions(fun))
          fills.add(new VariableFill(outBytes.position, arg))
          variables.getOrElseUpdate(arg, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case ext_fun_dat_2(fun, arg1, arg2) => {
          outBytes.put(0x33.asInstanceOf[Byte])
          outBytes.putShort(functions(fun))
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case ext_fun_ret(dst, fun) => {
          outBytes.put(0x35.asInstanceOf[Byte])
          outBytes.putShort(functions(fun))
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case ext_fun_ret_dat(dst, fun, arg) => {
          outBytes.put(0x36.asInstanceOf[Byte])
          outBytes.putShort(functions(fun))
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg))
          variables.getOrElseUpdate(arg, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case ext_fun_ret_dat_2(dst, fun, arg1, arg2) => {
          outBytes.put(0x37.asInstanceOf[Byte])
          outBytes.putShort(functions(fun))
          fills.add(new VariableFill(outBytes.position, dst))
          variables.getOrElseUpdate(dst, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg1))
          variables.getOrElseUpdate(arg1, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
          fills.add(new VariableFill(outBytes.position, arg2))
          variables.getOrElseUpdate(arg2, {variableCount += 1; variableCount - 1})
          outBytes.putInt(0)
        }
        case nop() =>
        case whitespace() =>
        case s: String => println("Unknown: " + s)
        case _ =>
      }
    }
    
    val endPos = outBytes.position()
    
    outBytes.flip()
    
    fills.toArray() foreach (_ match {
      case f: LabelByteFill => {
        if((labels(f.label) - f.loc - f.offset) > 127
            || (labels(f.label) - f.loc - f.offset) < -128) {
          println("attempting to branch too far to " + f.label);
        }
        outBytes.put(f.loc, (labels(f.label) - f.loc - f.offset).asInstanceOf[Byte])
      }
      case f: LabelIntFill => outBytes.putInt(f.loc, labels(f.label))
      case f: VariableFill => outBytes.putInt(f.loc, variables(f.name))
    })
    
    val finalBytes = new Array[Byte](endPos)
    outBytes.get(finalBytes)
    
    println(toHexString(finalBytes))
  }
  
  def parseHexString(hex: String): Array[Byte] = {
    if(hex == null) {
	  null
	}
	else {
	  val bytes = new Array[Byte](hex.length() / 2)
	  for(i <- 0 until bytes.length) {
	    var char1 = hex.charAt(i * 2).toInt
	    char1 = (if(char1 > 0x60) char1 - 0x57 else char1 - 0x30)
	    var char2 = hex.charAt(i * 2 + 1).toInt
	    char2 = (if(char2 > 0x60) char2 - 0x57 else char2 - 0x30)
	    if(char1 < 0 || char2 < 0 || char1 > 15 || char2 > 15)
	    {
	      throw new NumberFormatException("Invalid hex number: " + hex)
	    }
	    bytes(i) = ((char1 << 4) + char2).toByte
	  }
	  bytes
	}
  }
  
  val hexChars = Array( '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f' )
  
  def toHexString(bytes: Array[Byte]): String = {
    if(bytes == null) {
      null
    }
    else {
      val chars = new Array[Char](bytes.length * 2)
      for(i <- 0 until bytes.length) {
        chars(i * 2) = hexChars((bytes(i) >> 4) & 0x0F)
        chars(i * 2 + 1) = hexChars(bytes(i) & 0x0F)
      }
      String.valueOf(chars)
    }
  }
  
  def valToLong(hex: String): Long = {
    val buffer = ByteBuffer.allocate(8)
    buffer.order(ByteOrder.BIG_ENDIAN)
    buffer.put(parseHexString(hex))
    buffer.flip()
    buffer.getLong()
  }
}