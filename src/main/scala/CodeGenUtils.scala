package HT.CodeGen

import HT.{LabelAddrMap, MarchParameters}
import HT.Types.types

object FuncPlacement {
  var funcFirst: Boolean = false
  var funcBase: Long = 0
  var funcName: String = ""
}

def Offset(func: String, label: String): Long = {

  // lookup address from tryRun result and return offset
  val funcAddr = LabelAddrMap.get(func) match {
    case Some(addr) => addr
    case None => throw new Exception("Function not found " + func)
  }
  val labelAddr = LabelAddrMap.get(label) match {
    case Some(addr) => addr
    case None => throw new Exception("Label not found " + label)
  }
  val ret = labelAddr - funcAddr
  if (ret < 0) {
    throw new Exception("Negative offset " + ret)
  }
  ret
}

def CodeType2C(typ: types): String = {
  typ match {
    case types.Bool => "bool"
    case types.UInt => "unsigned int"
    case types.SInt => "int"
    case types.Addr => "int"
    case types.UInt64 => "uint64_t"
    case types.UInt32 => "uint32_t"
    case types.UInt16 => "uint16_t"
    case types.UInt8 => "uint8_t"
    case types.Int64 => "int64_t"
    case types.Int32 => "int32_t"
    case types.Int16 => "int16_t"
    case types.Int8 => "int8_t"
    case types.Ptr => "void *"
    case types.Atomic => "atomic_int"
    case types.Void => "void"
    //case types.Cacheline => "Cacheline"
    case _ => throw new Exception("Unsupported type " + typ)
  }
}

def CodeType2Z3(typ: types): String = {
  typ match {
    case types.Bool => "Bool"
    case types.SInt => "Int"
    case types.UInt => "Int"
    case types.Addr => "Int"
    //case types.Cacheline => "Cacheline"
    case _ => throw new Exception("Unsupported type " + typ)
  }
}
def Z3BodyPrefix(): String = {
  """
  |def solve():
  |    #Create an instance of a Z3 solver
  |    solver = Solver()
  |    solver.set("timeout", 10000)  # Set a timeout of 10 seconds
  |    #Declare variables
  |    constraints = [""".stripMargin
}
def Z3BodyPostfix(): String = {
  "]\n"
}

def Z3Prefix(): String = {
  """
    |from functools import reduce
    |from operator import xor
    |from z3 import *
    |set_option('smt.arith.random_initial_value', False)
    |set_option('smt.random_seed', 1)

    |""".stripMargin
}

def Z3PostfixLinker(): String = {
  """
    |    # Add constraints
    |    for _constrain in constraints:
    |        solver.add(_constrain)
    |
    |    # Check if the constraints are satisfiable
    |    if solver.check() == sat:
    |      #print("Satisfiable")
    |      # print(solver.model())
    |      model = solver.model()
    |      # Convert to concrete values for sorting
    |      sorted_vars = sorted(model.decls(), key=lambda d: model[d].as_long())
    |
    |      # Print or process sorted variables
    |      for var in sorted_vars:
    |        print(f". = {hex(model[var].as_long())};")
    |        print(f".{var.name()} : {{ *(.{var.name()})}}")
    |      exit(0)
    |
    |    else:
    |      print("Unsatisfiable")
    |      model = solver.model()
    |      # Convert to concrete values for sorting
    |      sorted_vars = sorted(model.decls(), key=lambda d: model[d].as_long())
    |      for var in sorted_vars:
    |        print(f"{var.name()} = {model[var]}")
    |solve()
    |""".stripMargin
}

def Z3Postfix(): String = {
  """
    |    # Add constraints
    |    for _constrain in constraints:
    |        solver.add(_constrain)
    |
    |    # Check if the constraints are satisfiable
    |    if solver.check() == sat:
    |      #print("Satisfiable")
    |      # print(solver.model())
    |      model = solver.model()
    |      # Convert to concrete values for sorting
    |      sorted_vars = sorted(model.decls(), key=lambda d: model[d].as_long())
    |
    |      # Print or process sorted variables
    |      for var in sorted_vars:
    |        print(f"{var.name()} = {model[var]}")
    |      exit(0)
    |
    |    else:
    |      print("Unsatisfiable")
    |      model = solver.model()
    |      # Convert to concrete values for sorting
    |      sorted_vars = sorted(model.decls(), key=lambda d: model[d].as_long())
    |      for var in sorted_vars:
    |        print(f"{var.name()} = {model[var]}")
    |solve()
    |""".stripMargin
}

def AMD64LinkerPrefix(): String = {
  """
    |ENTRY(_start)
    |SECTIONS
    |{
    PROVIDE (__executable_start = SEGMENT_START("text-segment", 0x400000)); . = SEGMENT_START("text-segment", 0x400000) + SIZEOF_HEADERS;
    |  PROVIDE_HIDDEN(__ehdr_start = .);
    |  .interp         : { *(.interp) }
    |  .note.gnu.build-id  : { *(.note.gnu.build-id) }
    |  .hash           : { *(.hash) }
    |  .gnu.hash       : { *(.gnu.hash) }
    |  .dynsym         : { *(.dynsym) }
    |  .dynstr         : { *(.dynstr) }
    |  .gnu.version    : { *(.gnu.version) }
    |  .gnu.version_d  : { *(.gnu.version_d) }
    |  .gnu.version_r  : { *(.gnu.version_r) }
    |  .rela.dyn       :
    |    {
    |      *(.rela.init)
    |      *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*)
    |      *(.rela.fini)
    |      *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*)
    |      *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*)
    |      *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*)
    |      *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*)
    |      *(.rela.ctors)
    |      *(.rela.dtors)
    |      *(.rela.got)
    |      *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*)
    |      *(.rela.ldata .rela.ldata.* .rela.gnu.linkonce.l.*)
    |      *(.rela.lbss .rela.lbss.* .rela.gnu.linkonce.lb.*)
    |      *(.rela.lrodata .rela.lrodata.* .rela.gnu.linkonce.lr.*)
    |      *(.rela.ifunc)
    |    }
    |  .rela.plt       :
    |    {
    |      *(.rela.plt)
    |      PROVIDE_HIDDEN (__rela_iplt_start = .);
    |      *(.rela.iplt)
    |      PROVIDE_HIDDEN (__rela_iplt_end = .);
    |    }
    |  .relr.dyn : { *(.relr.dyn) }
    |  . = ALIGN(CONSTANT (MAXPAGESIZE));
    |  .init           :
    |  {
    |    KEEP (*(SORT_NONE(.init)))
    |  }
    |  .plt            : { *(.plt) *(.iplt) }
    |.plt.got        : { *(.plt.got) }
    |.plt.sec        : { *(.plt.sec) }
    |  .text           :
    |  {
    |    *(.text.unlikely .text.*_unlikely .text.unlikely.*)
    |    *(.text.exit .text.exit.*)
    |    *(.text.startup .text.startup.*)
    |    *(.text.hot .text.hot.*)
    |    *(SORT(.text.sorted.*))
    |    *(.text .stub .text.* .gnu.linkonce.t.*)
    |    /* .gnu.warning sections are handled specially by elf.em.  */
    |    *(.gnu.warning)
    |  }
    |  .fini           :
    |  {
    |    KEEP (*(SORT_NONE(.fini)))
    |  }
    |  PROVIDE (__etext = .);
    |  PROVIDE (_etext = .);
    |  PROVIDE (etext = .);
    |  . = ALIGN(CONSTANT (MAXPAGESIZE));
    |  /* Adjust the address for the rodata segment.  We want to adjust up to
    |     the same address within the page on the next page up.  */
    |  . = SEGMENT_START("rodata-segment", ALIGN(CONSTANT (MAXPAGESIZE)) + (. & (CONSTANT (MAXPAGESIZE) - 1)));
    |  .rodata         : { *(.rodata .rodata.* .gnu.linkonce.r.*) }
    |  .rodata1        : { *(.rodata1) }
    |  .eh_frame_hdr   : { *(.eh_frame_hdr) *(.eh_frame_entry .eh_frame_entry.*) }
    |  .eh_frame       : ONLY_IF_RO { KEEP (*(.eh_frame)) *(.eh_frame.*) }
    |  .sframe         : ONLY_IF_RO { *(.sframe) *(.sframe.*) }
    |  .gcc_except_table   : ONLY_IF_RO { *(.gcc_except_table .gcc_except_table.*) }
    |  .gnu_extab   : ONLY_IF_RO { *(.gnu_extab*) }
    |  /* These sections are generated by the Sun/Oracle C++ compiler.  */
    |  .exception_ranges   : ONLY_IF_RO { *(.exception_ranges*) }
    |  /* Adjust the address for the data segment.  We want to adjust up to
    |     the same address within the page on the next page up.  */
    |  . = DATA_SEGMENT_ALIGN (CONSTANT (MAXPAGESIZE), CONSTANT (COMMONPAGESIZE));
    |  /* Exception handling  */
    |  .eh_frame       : ONLY_IF_RW { KEEP (*(.eh_frame)) *(.eh_frame.*) }
    |  .sframe         : ONLY_IF_RW { *(.sframe) *(.sframe.*) }
    |  .gnu_extab      : ONLY_IF_RW { *(.gnu_extab) }
    |  .gcc_except_table   : ONLY_IF_RW { *(.gcc_except_table .gcc_except_table.*) }
    |  .exception_ranges   : ONLY_IF_RW { *(.exception_ranges*) }
    |  /* Thread Local Storage sections  */
    |  .tdata	  :
    |   {
    |     PROVIDE_HIDDEN (__tdata_start = .);
    |     *(.tdata .tdata.* .gnu.linkonce.td.*)
    |   }
    |  .tbss		  : { *(.tbss .tbss.* .gnu.linkonce.tb.*) *(.tcommon) }
    |  .preinit_array    :
    |  {
    |    PROVIDE_HIDDEN (__preinit_array_start = .);
    |    KEEP (*(.preinit_array))
    |    PROVIDE_HIDDEN (__preinit_array_end = .);
    |  }
    |  .init_array    :
    |  {
    |    PROVIDE_HIDDEN (__init_array_start = .);
    |    KEEP (*(SORT_BY_INIT_PRIORITY(.init_array.*) SORT_BY_INIT_PRIORITY(.ctors.*)))
    |    KEEP (*(.init_array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o ) .ctors))
    |    PROVIDE_HIDDEN (__init_array_end = .);
    |  }
    |  .fini_array    :
    |  {
    |    PROVIDE_HIDDEN (__fini_array_start = .);
    |    KEEP (*(SORT_BY_INIT_PRIORITY(.fini_array.*) SORT_BY_INIT_PRIORITY(.dtors.*)))
    |    KEEP (*(.fini_array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o ) .dtors))
    |    PROVIDE_HIDDEN (__fini_array_end = .);
    |  }
    |  .ctors          :
    |  {
    |    /* gcc uses crtbegin.o to find the start of
    |       the constructors, so we make sure it is
    |       first.  Because this is a wildcard, it
    |       doesn't matter if the user does not
    |       actually link against crtbegin.o; the
    |       linker won't look for a file to match a
    |       wildcard.  The wildcard also means that it
    |       doesn't matter which directory crtbegin.o
    |       is in.  */
    |    KEEP (*crtbegin.o(.ctors))
    |    KEEP (*crtbegin?.o(.ctors))
    |    /* We don't want to include the .ctor section from
    |       the crtend.o file until after the sorted ctors.
    |       The .ctor section from the crtend file contains the
    |       end of ctors marker and it must be last */
    |    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .ctors))
    |    KEEP (*(SORT(.ctors.*)))
    |    KEEP (*(.ctors))
    |  }
    |  .dtors          :
    |  {
    |    KEEP (*crtbegin.o(.dtors))
    |    KEEP (*crtbegin?.o(.dtors))
    |    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .dtors))
    |    KEEP (*(SORT(.dtors.*)))
    |    KEEP (*(.dtors))
    |  }
    |  .jcr            : { KEEP (*(.jcr)) }
    |  .data.rel.ro : { *(.data.rel.ro.local* .gnu.linkonce.d.rel.ro.local.*) *(.data.rel.ro .data.rel.ro.* .gnu.linkonce.d.rel.ro.*) }
    |  .dynamic        : { *(.dynamic) }
    |  .got            : { *(.got) *(.igot) }
    |  . = DATA_SEGMENT_RELRO_END (SIZEOF (.got.plt) >= 24 ? 24 : 0, .);
    |  .got.plt        : { *(.got.plt) *(.igot.plt) }
    |  .data           :
    |  {
    |    *(.data .data.* .gnu.linkonce.d.*)
    |    SORT(CONSTRUCTORS)
    |  }
    |  .data1          : { *(.data1) }
    |  _edata = .; PROVIDE (edata = .);
    |  . = ALIGN(ALIGNOF(NEXT_SECTION));
    |  __bss_start = .;
    |  .bss            :
    |  {
    |   *(.dynbss)
    |   *(.bss .bss.* .gnu.linkonce.b.*)
    |   *(COMMON)
    |   /* Align here to ensure that the .bss section occupies space up to
    |      _end.  Align after .bss to ensure correct alignment even if the
    |      .bss section disappears because there are no input sections.
    |      FIXME: Why do we need it? When there is no .bss section, we do not
    |      pad the .data section.  */
    |   . = ALIGN(. != 0 ? 64 / 8 : 1);
    |  }
    |  .lbss   :
    |  {
    |    *(.dynlbss)
    |    *(.lbss .lbss.* .gnu.linkonce.lb.*)
    |    *(LARGE_COMMON)
    |  }
    |  . = ALIGN(64 / 8);
    |  . = SEGMENT_START("ldata-segment", .);
    |  .lrodata   ALIGN(CONSTANT (MAXPAGESIZE)) + (. & (CONSTANT (MAXPAGESIZE) - 1)) :
    |  {
    |    *(.lrodata .lrodata.* .gnu.linkonce.lr.*)
    |  }
    |  .ldata   ALIGN(CONSTANT (MAXPAGESIZE)) + (. & (CONSTANT (MAXPAGESIZE) - 1)) :
    |  {
    |    *(.ldata .ldata.* .gnu.linkonce.l.*)
    |    . = ALIGN(. != 0 ? 64 / 8 : 1);
    |  }
    |  . = ALIGN(64 / 8);
    |  _end = .; PROVIDE (end = .);
    |  . = DATA_SEGMENT_END (.);
    |  /* Stabs debugging sections.  */
    |  .stab          0 : { *(.stab) }
    |  .stabstr       0 : { *(.stabstr) }
    |  .stab.excl     0 : { *(.stab.excl) }
    |  .stab.exclstr  0 : { *(.stab.exclstr) }
    |  .stab.index    0 : { *(.stab.index) }
    |  .stab.indexstr 0 : { *(.stab.indexstr) }
    |  .comment 0 (INFO) : { *(.comment); LINKER_VERSION; }
    |  .gnu.build.attributes : { *(.gnu.build.attributes .gnu.build.attributes.*) }
    |
    |    /* begin custom part */
    |""".stripMargin
}

def RISCV64LinkerPrefix(): String = {
  s"""
     |ENTRY(_start)
     |SECTIONS
     |{
     |  /* Read-only sections, merged into text segment: */
     |  PROVIDE (__executable_start = SEGMENT_START("text-segment", 0x10000)); . = SEGMENT_START("text-segment", 0x10000) + SIZEOF_HEADERS;
     |  .interp         : { *(.interp) }
     |  .note.gnu.build-id  : { *(.note.gnu.build-id) }
     |  .hash           : { *(.hash) }
     |  .gnu.hash       : { *(.gnu.hash) }
     |  .dynsym         : { *(.dynsym) }
     |  .dynstr         : { *(.dynstr) }
     |  .gnu.version    : { *(.gnu.version) }
     |  .gnu.version_d  : { *(.gnu.version_d) }
     |  .gnu.version_r  : { *(.gnu.version_r) }
     |  .rela.dyn       :
     |    {
     |      *(.rela.init)
     |      *(.rela.text .rela.text.* .rela.gnu.linkonce.t.*)
     |      *(.rela.fini)
     |      *(.rela.rodata .rela.rodata.* .rela.gnu.linkonce.r.*)
     |      *(.rela.data .rela.data.* .rela.gnu.linkonce.d.*)
     |      *(.rela.tdata .rela.tdata.* .rela.gnu.linkonce.td.*)
     |      *(.rela.tbss .rela.tbss.* .rela.gnu.linkonce.tb.*)
     |      *(.rela.ctors)
     |      *(.rela.dtors)
     |      *(.rela.got)
     |      *(.rela.sdata .rela.sdata.* .rela.gnu.linkonce.s.*)
     |      *(.rela.sbss .rela.sbss.* .rela.gnu.linkonce.sb.*)
     |      *(.rela.sdata2 .rela.sdata2.* .rela.gnu.linkonce.s2.*)
     |      *(.rela.sbss2 .rela.sbss2.* .rela.gnu.linkonce.sb2.*)
     |      *(.rela.bss .rela.bss.* .rela.gnu.linkonce.b.*)
     |      *(.rela.ifunc)
     |    }
     |  .rela.plt       :
     |    {
     |      *(.rela.plt)
     |      PROVIDE_HIDDEN (__rela_iplt_start = .);
     |      *(.rela.iplt)
     |      PROVIDE_HIDDEN (__rela_iplt_end = .);
     |    }
     |  .init           :
     |  {
     |    KEEP (*(SORT_NONE(.init)))
     |  }
     |  .plt            : { *(.plt) *(.iplt) }
     |  .text           :
     |  {
     |    *(.text.unlikely .text.*_unlikely .text.unlikely.*)
     |    *(.text.exit .text.exit.*)
     |    *(.text.startup .text.startup.*)
     |    *(.text.hot .text.hot.*)
     |    *(SORT(.text.sorted.*))
     |    *(.text .stub .text.* .gnu.linkonce.t.*)
     |    /* .gnu.warning sections are handled specially by elf.em.  */
     |    *(.gnu.warning)
     |  }
     |  /* begin custom part */
  """.stripMargin
}

def AMD64LinkerPostfix(): String = {
  """

    /* DWARF debug sections.
    |     Symbols in the DWARF debugging sections are relative to the beginning
    |     of the section so we begin them at 0.  */
    |  /* DWARF 1.  */
    |  .debug          0 : { *(.debug) }
    |  .line           0 : { *(.line) }
    |  /* GNU DWARF 1 extensions.  */
    |  .debug_srcinfo  0 : { *(.debug_srcinfo) }
    |  .debug_sfnames  0 : { *(.debug_sfnames) }
    |  /* DWARF 1.1 and DWARF 2.  */
    |  .debug_aranges  0 : { *(.debug_aranges) }
    |  .debug_pubnames 0 : { *(.debug_pubnames) }
    |  /* DWARF 2.  */
    |  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
    |  .debug_abbrev   0 : { *(.debug_abbrev) }
    |  .debug_line     0 : { *(.debug_line .debug_line.* .debug_line_end) }
    |  .debug_frame    0 : { *(.debug_frame) }
    |  .debug_str      0 : { *(.debug_str) }
    |  .debug_loc      0 : { *(.debug_loc) }
    |  .debug_macinfo  0 : { *(.debug_macinfo) }
    |  /* SGI/MIPS DWARF 2 extensions.  */
    |  .debug_weaknames 0 : { *(.debug_weaknames) }
    |  .debug_funcnames 0 : { *(.debug_funcnames) }
    |  .debug_typenames 0 : { *(.debug_typenames) }
    |  .debug_varnames  0 : { *(.debug_varnames) }
    |  /* DWARF 3.  */
    |  .debug_pubtypes 0 : { *(.debug_pubtypes) }
    |  .debug_ranges   0 : { *(.debug_ranges) }
    |  /* DWARF 5.  */
    |  .debug_addr     0 : { *(.debug_addr) }
    |  .debug_line_str 0 : { *(.debug_line_str) }
    |  .debug_loclists 0 : { *(.debug_loclists) }
    |  .debug_macro    0 : { *(.debug_macro) }
    |  .debug_names    0 : { *(.debug_names) }
    |  .debug_rnglists 0 : { *(.debug_rnglists) }
    |  .debug_str_offsets 0 : { *(.debug_str_offsets) }
    |  .debug_sup      0 : { *(.debug_sup) }
    |  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
    |  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
    |}
    |""".stripMargin
}

def RISCV64LinkerPostfix(): String = {
  s"""
     |  . = ALIGN(0x100000);
     |
     |  .fini           :
     |  {
     |    KEEP (*(SORT_NONE(.fini)))
     |  }
     |  PROVIDE (__etext = .);
     |  PROVIDE (_etext = .);
     |  PROVIDE (etext = .);
     |  .rodata         : { *(.rodata .rodata.* .gnu.linkonce.r.*) }
     |  .rodata1        : { *(.rodata1) }
     |  .sdata2         :
     |  {
     |    *(.sdata2 .sdata2.* .gnu.linkonce.s2.*)
     |  }
     |  .sbss2          : { *(.sbss2 .sbss2.* .gnu.linkonce.sb2.*) }
     |  .eh_frame_hdr   : { *(.eh_frame_hdr) *(.eh_frame_entry .eh_frame_entry.*) }
     |  .eh_frame       : ONLY_IF_RO { KEEP (*(.eh_frame)) *(.eh_frame.*) }
     |  .sframe         : ONLY_IF_RO { *(.sframe) *(.sframe.*) }
     |  .gcc_except_table   : ONLY_IF_RO { *(.gcc_except_table .gcc_except_table.*) }
     |  .gnu_extab   : ONLY_IF_RO { *(.gnu_extab*) }
     |  /* These sections are generated by the Sun/Oracle C++ compiler.  */
     |  .exception_ranges   : ONLY_IF_RO { *(.exception_ranges*) }
     |  /* Adjust the address for the data segment.  We want to adjust up to
     |     the same address within the page on the next page up.  */
     |  . = DATA_SEGMENT_ALIGN (CONSTANT (MAXPAGESIZE), CONSTANT (COMMONPAGESIZE));
     |  /* Exception handling  */
     |  .eh_frame       : ONLY_IF_RW { KEEP (*(.eh_frame)) *(.eh_frame.*) }
     |  .sframe         : ONLY_IF_RW { *(.sframe) *(.sframe.*) }
     |  .gnu_extab      : ONLY_IF_RW { *(.gnu_extab) }
     |  .gcc_except_table   : ONLY_IF_RW { *(.gcc_except_table .gcc_except_table.*) }
     |  .exception_ranges   : ONLY_IF_RW { *(.exception_ranges*) }
     |  /* Thread Local Storage sections  */
     |  .tdata	  :
     |   {
     |     PROVIDE_HIDDEN (__tdata_start = .);
     |     *(.tdata .tdata.* .gnu.linkonce.td.*)
     |   }
     |  .tbss		  : { *(.tbss .tbss.* .gnu.linkonce.tb.*) *(.tcommon) }
     |  .preinit_array    :
     |  {
     |    PROVIDE_HIDDEN (__preinit_array_start = .);
     |    KEEP (*(.preinit_array))
     |    PROVIDE_HIDDEN (__preinit_array_end = .);
     |  }
     |  .init_array    :
     |  {
     |    PROVIDE_HIDDEN (__init_array_start = .);
     |    KEEP (*(SORT_BY_INIT_PRIORITY(.init_array.*) SORT_BY_INIT_PRIORITY(.ctors.*)))
     |    KEEP (*(.init_array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o ) .ctors))
     |    PROVIDE_HIDDEN (__init_array_end = .);
     |  }
     |  .fini_array    :
     |  {
     |    PROVIDE_HIDDEN (__fini_array_start = .);
     |    KEEP (*(SORT_BY_INIT_PRIORITY(.fini_array.*) SORT_BY_INIT_PRIORITY(.dtors.*)))
     |    KEEP (*(.fini_array EXCLUDE_FILE (*crtbegin.o *crtbegin?.o *crtend.o *crtend?.o ) .dtors))
     |    PROVIDE_HIDDEN (__fini_array_end = .);
     |  }
     |  .ctors          :
     |  {
     |    /* gcc uses crtbegin.o to find the start of
     |       the constructors, so we make sure it is
     |       first.  Because this is a wildcard, it
     |       doesn't matter if the user does not
     |       actually link against crtbegin.o; the
     |       linker won't look for a file to match a
     |       wildcard.  The wildcard also means that it
     |       doesn't matter which directory crtbegin.o
     |       is in.  */
     |    KEEP (*crtbegin.o(.ctors))
     |    KEEP (*crtbegin?.o(.ctors))
     |    /* We don't want to include the .ctor section from
     |       the crtend.o file until after the sorted ctors.
     |       The .ctor section from the crtend file contains the
     |       end of ctors marker and it must be last */
     |    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .ctors))
     |    KEEP (*(SORT(.ctors.*)))
     |    KEEP (*(.ctors))
     |  }
     |  .dtors          :
     |  {
     |    KEEP (*crtbegin.o(.dtors))
     |    KEEP (*crtbegin?.o(.dtors))
     |    KEEP (*(EXCLUDE_FILE (*crtend.o *crtend?.o ) .dtors))
     |    KEEP (*(SORT(.dtors.*)))
     |    KEEP (*(.dtors))
     |  }
     |  .jcr            : { KEEP (*(.jcr)) }
     |  .data.rel.ro : { *(.data.rel.ro.local* .gnu.linkonce.d.rel.ro.local.*) *(.data.rel.ro .data.rel.ro.* .gnu.linkonce.d.rel.ro.*) }
     |  .dynamic        : { *(.dynamic) }
     |  .got            : { *(.got) *(.igot) }
     |  . = DATA_SEGMENT_RELRO_END (SIZEOF (.got.plt) >= 16 ? 16 : 0, .);
     |  .got.plt        : { *(.got.plt) *(.igot.plt) }
     |  .data           :
     |  {
     |    __DATA_BEGIN__ = .;
     |    *(.data .data.* .gnu.linkonce.d.*)
     |    SORT(CONSTRUCTORS)
     |  }
     |  .data1          : { *(.data1) }
     |  /* We want the small data sections together, so single-instruction offsets
     |     can access them all, and initialized data all before uninitialized, so
     |     we can shorten the on-disk segment size.  */
     |  .sdata          :
     |  {
     |    __SDATA_BEGIN__ = .;
     |    *(.srodata.cst16) *(.srodata.cst8) *(.srodata.cst4) *(.srodata.cst2) *(.srodata .srodata.*)
     |    *(.sdata .sdata.* .gnu.linkonce.s.*)
     |  }
     |  _edata = .; PROVIDE (edata = .);
     |  . = ALIGN(ALIGNOF(NEXT_SECTION));
     |  __bss_start = .;
     |  .sbss           :
     |  {
     |    *(.dynsbss)
     |    *(.sbss .sbss.* .gnu.linkonce.sb.*)
     |    *(.scommon)
     |  }
     |  .bss            :
     |  {
     |   *(.dynbss)
     |   *(.bss .bss.* .gnu.linkonce.b.*)
     |   *(COMMON)
     |   /* Align here to ensure that the .bss section occupies space up to
     |      _end.  Align after .bss to ensure correct alignment even if the
     |      .bss section disappears because there are no input sections.
     |      FIXME: Why do we need it? When there is no .bss section, we do not
     |      pad the .data section.  */
     |   . = ALIGN(. != 0 ? 64 / 8 : 1);
     |  }
     |. = ALIGN(64 / 8);
     |  . = SEGMENT_START("ldata-segment", .);
     |  . = ALIGN(64 / 8);
     |  __BSS_END__ = .;
     |    __global_pointer$$ = MIN(__SDATA_BEGIN__ + 0x800,
     |		            MAX(__DATA_BEGIN__ + 0x800, __BSS_END__ - 0x800));
     |  _end = .; PROVIDE (end = .);
     |  . = DATA_SEGMENT_END (.);
     |  /* Stabs debugging sections.  */
     |  .stab          0 : { *(.stab) }
     |  .stabstr       0 : { *(.stabstr) }
     |  .stab.excl     0 : { *(.stab.excl) }
     |  .stab.exclstr  0 : { *(.stab.exclstr) }
     |  .stab.index    0 : { *(.stab.index) }
     |  .stab.indexstr 0 : { *(.stab.indexstr) }
     |  .comment 0 (INFO) : { *(.comment); LINKER_VERSION; }
     |  .gnu.build.attributes : { *(.gnu.build.attributes .gnu.build.attributes.*) }
     |  /* DWARF debug sections.
     |     Symbols in the DWARF debugging sections are relative to the beginning
     |     of the section so we begin them at 0.  */
     |  /* DWARF 1.  */
     |  .debug          0 : { *(.debug) }
     |  .line           0 : { *(.line) }
     |  /* GNU DWARF 1 extensions.  */
     |  .debug_srcinfo  0 : { *(.debug_srcinfo) }
     |  .debug_sfnames  0 : { *(.debug_sfnames) }
     |  /* DWARF 1.1 and DWARF 2.  */
     |  .debug_aranges  0 : { *(.debug_aranges) }
     |  .debug_pubnames 0 : { *(.debug_pubnames) }
     |  /* DWARF 2.  */
     |  .debug_info     0 : { *(.debug_info .gnu.linkonce.wi.*) }
     |  .debug_abbrev   0 : { *(.debug_abbrev) }
     |  .debug_line     0 : { *(.debug_line .debug_line.* .debug_line_end) }
     |  .debug_frame    0 : { *(.debug_frame) }
     |  .debug_str      0 : { *(.debug_str) }
     |  .debug_loc      0 : { *(.debug_loc) }
     |  .debug_macinfo  0 : { *(.debug_macinfo) }
     |  /* SGI/MIPS DWARF 2 extensions.  */
     |  .debug_weaknames 0 : { *(.debug_weaknames) }
     |  .debug_funcnames 0 : { *(.debug_funcnames) }
     |  .debug_typenames 0 : { *(.debug_typenames) }
     |  .debug_varnames  0 : { *(.debug_varnames) }
     |  /* DWARF 3.  */
     |  .debug_pubtypes 0 : { *(.debug_pubtypes) }
     |  .debug_ranges   0 : { *(.debug_ranges) }
     |  /* DWARF 5.  */
     |  .debug_addr     0 : { *(.debug_addr) }
     |  .debug_line_str 0 : { *(.debug_line_str) }
     |  .debug_loclists 0 : { *(.debug_loclists) }
     |  .debug_macro    0 : { *(.debug_macro) }
     |  .debug_names    0 : { *(.debug_names) }
     |  .debug_rnglists 0 : { *(.debug_rnglists) }
     |  .debug_str_offsets 0 : { *(.debug_str_offsets) }
     |  .debug_sup      0 : { *(.debug_sup) }
     |  .gnu.attributes 0 : { KEEP (*(.gnu.attributes)) }
     |  /DISCARD/ : { *(.note.GNU-stack) *(.gnu_debuglink) *(.gnu.lto_*) }
     |}
  """.stripMargin
}

def LinkerPrefix() = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64LinkerPrefix()
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64LinkerPrefix()
  } else {
    throw new Exception("Unsupported ISA")
  }
}

def LinkerTryRun() = {
  s"""
  |  . = ALIGN(0x100000);
  |  .customtext : { *(.customtext .customtext.*) }
  |  .customdata : { *(.customdata .customdata.*) }
  """.stripMargin
}

def LinkerPostfix() = {
  if (MarchParameters.ISA == "x86_64") {
    AMD64LinkerPostfix()
  } else if (MarchParameters.ISA == "riscv64") {
    RISCV64LinkerPostfix()
  } else {
    throw new Exception("Unsupported ISA")
  }
}

def getLinker(custom: String): String = {
  LinkerPrefix() + custom + LinkerPostfix()
}