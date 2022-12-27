import mill._
import mill.scalalib._
import mill.define.{TaskModule, Command}
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.scalalib.TestModule.Utest
import coursier.maven.MavenRepository
import $file.dependencies.chisel3.build
import $file.dependencies.firrtl.build
import $file.dependencies.treadle.build
import $file.dependencies.chiseltest.build
import $file.dependencies.arithmetic.common
import $file.dependencies.tilelink.common
import $file.common

object v {
  val scala = "2.13.6"
  val utest = ivy"com.lihaoyi::utest:latest.integration"
  val mainargs = ivy"com.lihaoyi::mainargs:0.3.0"
  // for arithmetic
  val upickle = ivy"com.lihaoyi::upickle:latest.integration"
  val osLib = ivy"com.lihaoyi::os-lib:latest.integration"
  val bc = ivy"org.bouncycastle:bcprov-jdk15to18:latest.integration"
  val spire = ivy"org.typelevel::spire:latest.integration"
  val evilplot = ivy"io.github.cibotech::evilplot:latest.integration"
}

object myfirrtl extends dependencies.firrtl.build.firrtlCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "firrtl"

  override val checkSystemAntlr4Version = false
  override val checkSystemProtocVersion = false
  override val protocVersion = os.proc("protoc", "--version").call().out.text.dropRight(1).split(' ').last
  override val antlr4Version = os.proc("antlr4").call().out.text.split('\n').head.split(' ').last
}

object mytreadle extends dependencies.treadle.build.treadleCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "treadle"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
}

object mychisel3 extends dependencies.chisel3.build.chisel3CrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chisel3"

  def firrtlModule: Option[PublishModule] = Some(myfirrtl)

  def treadleModule: Option[PublishModule] = Some(mytreadle)

  def chiseltestModule: Option[PublishModule] = Some(mychiseltest)
}

object mychiseltest extends dependencies.chiseltest.build.chiseltestCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chiseltest"

  def chisel3Module: Option[PublishModule] = Some(mychisel3)

  def treadleModule: Option[PublishModule] = Some(mytreadle)
}

object myarithmetic extends dependencies.arithmetic.common.ArithmeticModule {
  override def millSourcePath = os.pwd / "dependencies" / "arithmetic" / "arithmetic"

  def scalaVersion = T {
    v.scala
  }

  def chisel3Module: Option[PublishModule] = Some(mychisel3)

  def chisel3PluginJar = T {
    Some(mychisel3.plugin.jar())
  }

  def chiseltestModule = Some(mychiseltest)

  def upickle: T[Dep] = v.upickle

  def osLib: T[Dep] = v.osLib

  def spire: T[Dep] = v.spire

  def evilplot: T[Dep] = v.evilplot

  def bc: T[Dep] = v.bc

  def utest: T[Dep] = v.utest
}

object mytilelink extends dependencies.tilelink.common.TileLinkModule {
  override def millSourcePath = os.pwd / "dependencies" / "tilelink" / "tilelink"

  def scalaVersion = T {
    v.scala
  }

  def chisel3Module: Option[PublishModule] = Some(mychisel3)

  def chisel3PluginJar = T {
    Some(mychisel3.plugin.jar())
  }
}

object vector extends common.VectorModule with ScalafmtModule {
  m =>
  def millSourcePath = os.pwd / "v"

  def scalaVersion = T {
    v.scala
  }

  def chisel3Module = Some(mychisel3)

  def chisel3PluginJar = T {
    Some(mychisel3.plugin.jar())
  }

  def chiseltestModule = Some(mychiseltest)

  def arithmeticModule = Some(myarithmetic)

  def tilelinkModule = Some(mytilelink)

  def utest: T[Dep] = v.utest
}

object tests extends Module {
  object elaborate extends ScalaModule {
    override def scalacPluginClasspath = T {
      Agg(mychisel3.plugin.jar())
    }

    override def scalacOptions = T {
      super.scalacOptions() ++ Some(mychisel3.plugin.jar()).map(path => s"-Xplugin:${path.path}") ++ Seq("-Ymacro-annotations")
    }

    override def scalaVersion = v.scala

    override def moduleDeps = Seq(vector)

    override def ivyDeps = T {
      Seq(
        v.mainargs
      )
    }

    def elaborate = T {
      // class path for `moduleDeps` is only a directory, not a jar, which breaks the cache.
      // so we need to manually add the class files of `moduleDeps` here.
      upstreamCompileOutput()
      mill.modules.Jvm.runLocal(
        finalMainClass(),
        runClasspath().map(_.path),
        Seq(
          "--dir", T.dest.toString,
        ),
      )
      PathRef(T.dest)
    }

    def chiselAnno = T {
      os.walk(elaborate().path).collectFirst { case p if p.last.endsWith("anno.json") => p }.map(PathRef(_)).get
    }

    def chirrtl = T {
      os.walk(elaborate().path).collectFirst { case p if p.last.endsWith("fir") => p }.map(PathRef(_)).get
    }

    def topName = T {
      chirrtl().path.last.split('.').head
    }

  }

  object mfccompile extends Module {

    def compile = T {
      os.proc("firtool",
        elaborate.chirrtl().path,
        s"--annotation-file=${elaborate.chiselAnno().path}",
        "-disable-infer-rw",
        "-dedup",
        "-O=debug",
        "--split-verilog",
        "--preserve-values=named",
        "--output-annotation-file=mfc.anno.json",
        "--lowering-options=verifLabels",
        s"-o=${T.dest}"
      ).call(T.dest)
      PathRef(T.dest)
    }

    def rtls = T {
      os.read(compile().path / "filelist.f").split("\n").map(str =>
        try {
          os.Path(str)
        } catch {
          case e: IllegalArgumentException if e.getMessage.contains("is not an absolute path") =>
            compile().path / str.stripPrefix("./")
        }
      ).filter(p => p.ext == "v" || p.ext == "sv").map(PathRef(_)).toSeq
    }

    def annotations = T {
      os.walk(compile().path).filter(p => p.last.endsWith("mfc.anno.json")).map(PathRef(_))
    }
  }

  object emulator extends Module {

    def csrcDir = T.source {
      PathRef(millSourcePath / "src")
    }

    def allCHeaderFiles = T.sources {
      os.walk(csrcDir().path).filter(_.ext == "h").map(PathRef(_))
    }

    def allCSourceFiles = T.sources {
      Seq(
        "spike_event.cc",
        "vbridge_impl.cc",
        "dpi.cc",
      ).map(f => PathRef(csrcDir().path / f))
    }

    def verilatorArgs = T {
      Seq(
        // format: off
        "--x-initial unique",
        "--output-split 100000",
        "--max-num-width 1048576",
        "--main",
        "--timing",
        // use for coverage
        "--coverage-user",
        "--assert",
        // format: on
      )
    }

    def topName = T {
      "V"
    }

    def verilatorThreads = T {
      8
    }

    def CMakeListsString = T {
      // format: off
      s"""cmake_minimum_required(VERSION 3.20)
         |project(emulator)
         |set(CMAKE_CXX_STANDARD 17)
         |
         |find_package(args REQUIRED)
         |find_package(glog REQUIRED)
         |find_package(fmt REQUIRED)
         |find_package(libspike REQUIRED)
         |find_package(verilator REQUIRED)
         |find_package(Threads REQUIRED)
         |set(THREADS_PREFER_PTHREAD_FLAG ON)
         |
         |add_executable(emulator
         |${allCSourceFiles().map(_.path).mkString("\n")}
         |)
         |
         |target_include_directories(emulator PUBLIC ${csrcDir().path.toString})
         |
         |target_link_libraries(emulator PUBLIC $${CMAKE_THREAD_LIBS_INIT})
         |target_link_libraries(emulator PUBLIC libspike fmt glog)  # note that libargs is header only, nothing to link
         |target_compile_definitions(emulator PRIVATE COSIM_VERILATOR)
         |
         |verilate(emulator
         |  SOURCES
         |  ${mfccompile.rtls().map(_.path.toString).mkString("\n")}
         |  TRACE_FST
         |  TOP_MODULE ${elaborate.topName()}
         |  PREFIX V${elaborate.topName()}
         |  OPT_FAST
         |  THREADS ${verilatorThreads()}
         |  VERILATOR_ARGS ${verilatorArgs().mkString(" ")}
         |)
         |""".stripMargin
      // format: on
    }

    def cmakefileLists = T {
      val path = T.dest / "CMakeLists.txt"
      os.write.over(path, CMakeListsString())
      PathRef(T.dest)
    }

    def buildDir = T {
      PathRef(T.dest)
    }

    def config = T {
      mill.modules.Jvm.runSubprocess(Seq("cmake",
        "-G", "Ninja",
        "-S", cmakefileLists().path,
        "-B", buildDir().path
      ).map(_.toString), Map[String, String](), T.dest)
    }

    def elf = T {
      // either rtl or testbench change should trigger elf rebuild
      mfccompile.rtls()
      allCSourceFiles()
      allCHeaderFiles()
      config()
      mill.modules.Jvm.runSubprocess(Seq("ninja", "-C", buildDir().path).map(_.toString), Map[String, String](), buildDir().path)
      PathRef(buildDir().path / "emulator")
    }
  }

  object cases extends Module {
    c =>
    trait Case extends Module {
      def name: String = millOuterCtx.segment.pathSegments.last

      def sources = T.sources {
        millSourcePath
      }

      def allSourceFiles = T {
        Lib.findSourceFiles(sources(), Seq("S", "s", "c", "cpp")).map(PathRef(_))
      }

      def includeDir = T {
        Seq[PathRef]()
      }

      def linkOpts = T {
        Seq("-mno-relax")
      }

      def linkScript: T[PathRef] = T {
        os.write(T.ctx.dest / "linker.ld",
          s"""
             |SECTIONS
             |{
             |  . = 0x1000;
             |  .text.start : { *(.text.start) }
             |}
             |""".stripMargin)
        PathRef(T.ctx.dest / "linker.ld")
      }

      def elf: T[PathRef] = T {
        os.proc(Seq("clang-rv32", "-o", name + ".elf", "-mabi=ilp32f", "-march=rv32gcv", s"-T${linkScript().path}") ++ linkOpts() ++ includeDir().map(p => s"-I${p.path}") ++ allSourceFiles().map(_.path.toString)).call(T.ctx.dest)
        PathRef(T.ctx.dest / (name + ".elf"))
      }

      def bin: T[PathRef] = T {
        os.proc(Seq("llvm-objcopy", "-O", "binary", "--only-section=.text", elf().path.toString, name)).call(T.ctx.dest)
        PathRef(T.ctx.dest / name)
      }
    }

    object `riscv-vector-tests` extends Module {
      u =>
      override def millSourcePath = os.pwd / "dependencies" / "riscv-vector-tests"

      def allTests = os.walk(millSourcePath / "configs").filter(_.ext == "toml").map(_.last.replace(".toml", ""))

      def allGoSources = T.sources {
        os.walk(millSourcePath).filter(f => f.ext == "go" || f.last == "go.mod").map(PathRef(_))
      }

      def asmGenerator = T {
        // depends on GO
        allGoSources()
        val elf = T.dest / "generator"
        os.proc("go", "build", "-o", elf, "single/single.go").call(cwd = millSourcePath)
        PathRef(elf)
      }

      object ut extends mill.Cross[ut](allTests: _*)

      class ut(caseName: String) extends c.Case {
        override def name = caseName

        override def includeDir = T {
          Seq(
            u.millSourcePath / "env" / "sequencer-vector",
            u.millSourcePath / "macros" / "sequencer-vector"
          ).map(PathRef(_))
        }

        override def linkOpts = T {
          Seq("-mno-relax", "-static", "-mcmodel=medany", "-fvisibility=hidden", "-nostdlib")
        }

        override def linkScript: T[PathRef] = T.source {
          PathRef(u.millSourcePath / "env" / "sequencer-vector" / "link.ld")
        }

        override def allSourceFiles: T[Seq[PathRef]] = T {
          val f = T.dest / s"${name.replace('_', '.')}.S"
          os.proc(
            asmGenerator().path,
            "-VLEN", 1024,
            "-XLEN", 32,
            "-outputfile", f,
            "-configfile", u.millSourcePath / "configs" / s"${name.replace('_', '.')}.toml"
          ).call(T.dest)
          Seq(PathRef(f))
        }

        override def bin: T[PathRef] = T {
          os.proc(Seq("llvm-objcopy", "-O", "binary", elf().path.toString, name)).call(T.ctx.dest)
          PathRef(T.ctx.dest / name)
        }
      }
    }

    object smoketest extends Case

    object mmm extends Case
  }

  object run extends mill.Cross[run]((cases.`riscv-vector-tests`.allTests ++ Seq(cases.smoketest, cases.mmm).map(_.name)): _*)

  class run(name: String) extends Module with TaskModule {
    override def defaultCommandName() = "run"

    def caseToRun = name match {
      case "smoketest" => cases.smoketest
      case "mmm" => cases.mmm
      case _ => cases.`riscv-vector-tests`.ut(name)
    }

    def run(args: String*) = T.command {
      def envDefault(name: String, default: String) = {
        name -> args.map(_.split("=")).collectFirst {
          case arg if arg.head == name => arg.last
        }.getOrElse(default)
      }

      val runEnv = Map(
        "COSIM_bin" -> caseToRun.bin().path.toString,
        "COSIM_wave" -> (T.dest / "wave").toString,
        "COSIM_reset_vector" -> "1000",
        envDefault("COSIM_timeout", "1000000"),
        envDefault("GLOG_logtostderr", "1"),
      )
      T.log.info(s"run test: ${caseToRun.name} with:\n ${runEnv.map { case (k, v) => s"$k=$v" }.mkString(" ")} ${tests.emulator.elf().path.toString}")
      os.proc(Seq(tests.emulator.elf().path.toString)).call(env = runEnv)
      PathRef(T.dest)
    }
  }
}
