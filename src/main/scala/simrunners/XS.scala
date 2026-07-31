package HT.simrunners

import java.nio.file.{Files, Paths, StandardCopyOption}
import java.io.{File, PrintWriter}
import java.nio.file.Path
import scala.sys.process.Process
import HT.TmpFiles
import HT.observation.GlobalCycle
import HT.simrunners.XSRunnerParams.RISCV_LINUX_HOME

import scala.sys.process.ProcessLogger

val userName = "xim-intel14"

var emu_path = "emu"

object XSRunnerParams {
    //env vars:
    val RISCV_LINUX_HOME = s"/root/ELF/linux-6.10.3"
    val RISCV_ROOTFS_HOME = s"/root/ELF/riscv-rootfs"
    val WORKLOAD_BUILD_ENV_HOME = s"/root/ELF/nemu_board"
    val OPENSBI_HOME = s"/root/ELF/opensbi/"
    val ARCH = "riscv"
    val CROSS_COMPILE = "riscv64-linux-gnu-"

    val LibCheckpoint_HOME = s"/root/ELF/LibCheckpointAlpha"
    val Nemu_BUILD = s"/root/ELF/NEMU/build"
    val XS_emu = s"/root/XS"
    val vcd_dir = s"/root/XS/build"
}

def XSRun(max_insts: Long, dumpStartCycle: Option[Int] = None, dumpEndCycle: Option[Int] = None, customELF: Option[String] = None): Path = {

    if (dumpStartCycle.isDefined) {
        GlobalCycle = dumpStartCycle.get
    }
    
    //Step 0: Copy the executable to rootfs

    val source = if (customELF.isDefined) Paths.get(customELF.get) else TmpFiles.outDest
    val destination = Paths.get(s"${XSRunnerParams.RISCV_ROOTFS_HOME}/hello")

    // Delete the previous hello file if it exists
    val helloFile = new File(destination.toString)
    if (helloFile.exists()) {
        helloFile.delete()
    }

    // Copy the new file
    Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING)


    //Common settings for build commands

    val environment = Map(
        "ARCH" -> XSRunnerParams.ARCH,
        "CROSS_COMPILE" -> XSRunnerParams.CROSS_COMPILE,
        "RISCV_LINUX_HOME" -> XSRunnerParams.RISCV_LINUX_HOME,
        "RISCV_ROOTFS_HOME" -> XSRunnerParams.RISCV_ROOTFS_HOME,
        "WORKLOAD_BUILD_ENV_HOME" -> XSRunnerParams.WORKLOAD_BUILD_ENV_HOME,
        "OPENSBI_HOME" -> XSRunnerParams.OPENSBI_HOME
    )

    //Step 1: Build the Linux kernel

    val ecode = Process("make -j12", Some(new File(XSRunnerParams.RISCV_LINUX_HOME)), environment.toSeq *).!
    assert(ecode == 0, s"Make command failed with exit code $ecode")

    //Step 2: Build the OpenSBI

    val opensbi_build_cmd = s"make PLATFORM=generic FW_PAYLOAD_PATH=${XSRunnerParams.RISCV_LINUX_HOME}/arch/riscv/boot/Image FW_FDT_PATH=${XSRunnerParams.WORKLOAD_BUILD_ENV_HOME}/dts/build/xiangshan.dtb FW_PAYLOAD_OFFSET=0x100000  -j10"
    val ecode2 = Process(opensbi_build_cmd, Some(new File(XSRunnerParams.OPENSBI_HOME)), environment.toSeq *).!
    assert(ecode2 == 0, s"OpenSBI build command failed with exit code $ecode2")

    //Step 3: build LibCheckpoint
    //make clean && make GCPT_PAYLOAD_PATH=$OPENSBI_HOME/build/platform/generic/firmware/fw_payload.bin
    val libcheckpoint_build_cmd = s"make GCPT_PAYLOAD_PATH=${XSRunnerParams.OPENSBI_HOME}/build/platform/generic/firmware/fw_payload.bin -j10"

    val ecode_3_1 = Process("make clean", Some(new File(XSRunnerParams.LibCheckpoint_HOME)), environment.toSeq *).!
    assert(ecode_3_1 == 0, s"Make clean command failed with exit code $ecode_3_1")
    val ecode_3_2 = Process(libcheckpoint_build_cmd, Some(new File(XSRunnerParams.LibCheckpoint_HOME)), environment.toSeq *).!
    assert(ecode_3_2 == 0, s"LibCheckpoint build command failed with exit code $ecode_3_2")

    //Step 4: Generate the checkpoint using NEMU

    val libcheckpoint_bin_location = s"${XSRunnerParams.LibCheckpoint_HOME}/build/gcpt.bin"

    //val nemu_generate_chkpoint_cmd = s"./riscv64-nemu-interpreter $libcheckpoint_bin_location -D ./checkpoint_example_result -w bbl -C uniform -b -u --cpt-interval 1 --checkpoint-format zstd -r ../resource/gcpt_restore/build/gcpt.bin"
    val nemu_generate_chkpoint_cmd = s"./riscv64-nemu-interpreter $libcheckpoint_bin_location -D ${TmpFiles.nemu_path.toString} -w bbl -C uniform -b -u --cpt-interval 1 --checkpoint-format zstd -r ../resource/gcpt_restore/build/gcpt.bin"
    val ecode_4 = Process(nemu_generate_chkpoint_cmd, Some(new File(XSRunnerParams.Nemu_BUILD)), environment.toSeq *).!
    assert(ecode_4 == 0, s"NEMU generate checkpoint command failed with exit code $ecode_4")


    //Step 4.5: Find the checkpoint location
    val checkpointDir = TmpFiles.nemu_path.resolve("uniform").resolve("bbl").resolve("1")
    val zstdFiles = checkpointDir.toFile.listFiles().filter(_.getName.endsWith(".zstd"))

    val checkpoint_loc =
        if (zstdFiles.length == 1) {
            zstdFiles.head.toPath
        } else {
            throw new RuntimeException(s"Expected exactly one .zstd file in $checkpointDir, but found ${zstdFiles.length}")
        }

    //Step 5: Run the verilator simulation with the given checkpoint
    // --diff ready-to-run/riscv64-nemu-interpreter-so
    val run_cmd = s"./build/${emu_path} -i ${checkpoint_loc.toString} --no-diff --dump-wave -I ${max_insts.toString}  ${if (dumpEndCycle.isDefined) s" -e ${dumpEndCycle.get}" else ""} ${if (dumpStartCycle.isDefined) s" -b ${dumpStartCycle.get}" else ""}"
    println(s"run cmd ${run_cmd}")
    val perfLog = new PrintWriter(new File("/tmp/perf.txt"))
    val ecode_5 =
        try {
            Process(run_cmd, Some(new File(XSRunnerParams.XS_emu)), environment.toSeq *).!(
                ProcessLogger(stdout => println(stdout), stderr => perfLog.println(stderr))
            )
        } finally {
            perfLog.close()
        }
    if (ecode_5 != 0) then println( s"XiangShan emulation command ended with exit code $ecode_5, please verify that that was expected")

    println("Finished VCD generation successfully!!")


    // Find the latest modified .vcd file in the vcd_dir
    val vcdDir = new File(XSRunnerParams.vcd_dir)
    val vcdFiles = vcdDir.listFiles().filter(_.getName.endsWith(".vcd"))

    val latestVcdFile =
        if (vcdFiles.nonEmpty) {
            vcdFiles.maxBy(_.lastModified()).toPath
        } else {
            throw new RuntimeException(s"No .vcd files found in ${XSRunnerParams.vcd_dir}")
        }

    println(s"Latest VCD file: $latestVcdFile")
    latestVcdFile

}
