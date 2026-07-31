package HT.macOS

// this file is used to generate the OS-specific code for macOS

// Note that macOS seems to not accept linker script
// which makes our work much harder
// TODO: implement macOS support

def sectionStringASM(section: String): String = {
  s"""
  |section .${section}:
  |""".stripMargin
}

def sectionStringC(section: String): String = {
  s"""
  |__attribute__((section("__TEXT,__${section}")))
  |""".stripMargin
}