package HT

import HT.AsmPatcher.AsmInserter
import HT.branchMap
import HT.MarchParameters
import HT.simrunners.userName

import java.io.{File, PrintWriter}
import java.nio.file.Path


val gcc_param = "-fno-asynchronous-unwind-tables -fno-unwind-tables -fno-exceptions -O0 -S"


val PATH = "/llvm-dsl/bin/"

val patchScriptPrefix = s"/root/HT/src/main/python"

import java.nio.file.{Files, Paths}
import scala.util.{Try, Random}

val PythonHome = "/usr/"

object TmpFiles {
  val base_dir : Path = {
    var attempt = 0
    var result: Option[java.nio.file.Path] = None

    while (attempt < 5 && result.isEmpty) {
      val path = Paths.get(s"/tmp/ga_run_${Random.nextInt(100000)}")
      result = Try(Files.createDirectory(path)).toOption
      attempt += 1
    }

    result.getOrElse(throw new RuntimeException("Failed to create directory after 5 attempts"))
  }

  val GAAsm: Path = base_dir.resolve("GA.s")
  val GASedAsm: Path = base_dir.resolve("GAnew.s")
  val patchedAsm: Path = base_dir.resolve("GApatched.s")
  val outFile: Path = base_dir.resolve("Ga.c")
  val outDest: Path = base_dir.resolve("Ga.elf")
  val linkerDest: Path = base_dir.resolve("linker.ld")
  val figureFile: Path = base_dir.resolve("timeline.txt")
  val htmlFile: Path = base_dir.resolve("index.html")

  val try_tmp_file: Path = base_dir.resolve("GATry.c")
  val try_asm_file: Path = base_dir.resolve("GATry.s")
  val try_patched_asm_file: Path = base_dir.resolve("GATryPatched.s")
  val try_out: Path = base_dir.resolve("GATry.elf")
  val try_linker: Path = base_dir.resolve("GATry.ld")

  val nemu_path: Path = base_dir.resolve("nemu_checkpoint")

}

val sed_param = s"sed '/\\.size/d' ${TmpFiles.GAAsm.toString}"

def outputGen(code: String, linker: String) = {
  val outFile = TmpFiles.outFile.toString
  val outDest = TmpFiles.outDest.toString

  val linkerDest = TmpFiles.linkerDest.toString

  val writer = new PrintWriter(new File(outFile))
  writer.write(code)
  writer.close()

  val linkerWriter = new PrintWriter(new File(linkerDest))
  linkerWriter.write(linker)
  linkerWriter.close()

  // compile command
  val ppp = TmpFiles.GAAsm.toString
  val compile = s"${PATH}${MarchParameters.CrossPrefix}clang ${MarchParameters.compileTarget} $gcc_param $outFile -o $ppp" // will generate GA.s file

  // sed command
  val sed = s"$sed_param" // will generate GAnew.s file

  // final compile command
  val patched_asm_path = TmpFiles.patchedAsm.toString
  val finalCompile = s"${PATH}${MarchParameters.CrossPrefix}clang -Wl,--no-relax ${MarchParameters.compileTarget} -static -o $outDest $patched_asm_path -T $linkerDest"

  // execute the commands
  val compileResult = sys.process.Process(compile).!
  val sedResult = sys.process.Process(sed).!
  // redirect sed output to GAnew.s
  val sedOutput = sys.process.Process(sed).!!
  val sedWriter = new PrintWriter(TmpFiles.GASedAsm.toFile)
  sedWriter.write(sedOutput)
  sedWriter.close()

  // patch branch instructions with section directive
  //TODO: change AdmInserter to use path objects instead of strings
  AsmInserter.insertIntoAsmFile(TmpFiles.GASedAsm.toString, TmpFiles.patchedAsm.toString, branchMap)

  val finalCompileResult = sys.process.Process(finalCompile).!

  /*
  println("compiler result: " + compileResult)
  println("sed result: " + sedResult)
  println("final compile result: " + finalCompileResult)
  */



  val patchScript = if (MarchParameters.ISA == "x86_64") {
    "patchnop.py"
  } else {
    "rv-patchnop.py"
  }

  val tmpOutputDest = TmpFiles.base_dir.resolve("output.elf").toString
  val pythonCmd = s"${PythonHome}/bin/python3 ${patchScriptPrefix}/${patchScript} $outDest $tmpOutputDest"
  val pythonResult = sys.process.Process(pythonCmd).!
  println("NOP patch script result: " + pythonResult)
  if (pythonResult != 0) {
    throw new RuntimeException(s"Python patching script failed with exit code $pythonResult")
  }

  // 2. Move the output file from /tmp/output.elf to /tmp/Ga.elf
  val moveCmd = s"mv $tmpOutputDest $outDest"
  val moveResult = sys.process.Process(moveCmd).!
  println("Move command result: " + moveResult)

  // 3. Add execute permission to /tmp/Ga.elf
  val chmodCmd = s"chmod +x $outDest"
  val chmodResult = sys.process.Process(chmodCmd).!
  println("Chmod command result: " + chmodResult)
}