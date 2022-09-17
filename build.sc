import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt._
import mill.scalalib.TestModule.Utest
import coursier.maven.MavenRepository
import $file.dependencies.chisel3.build
import $file.dependencies.firrtl.build
import $file.dependencies.treadle.build
import $file.dependencies.chiseltest.build
import $file.common

object v {
  val scala = "2.12.16"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.6-SNAPSHOT"
  val chisel3Plugin = ivy"edu.berkeley.cs::chisel3-plugin:3.6-SNAPSHOT"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:3.6-SNAPSHOT"
  val utest = ivy"com.lihaoyi::utest:latest.integration"
}

object myfirrtl extends dependencies.firrtl.build.firrtlCrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "firrtl"
  override val checkSystemAntlr4Version = false
  override val checkSystemProtocVersion = false
  override val protocVersion = os.proc("protoc", "--version").call().out.text.dropRight(1).split(' ').last
  override val antlr4Version = os.proc("antlr4").call().out.text.split('\n').head.split(' ').last
}
object mytreadle extends dependencies.treadle.build.treadleCrossModule(v.scala) {
  override def millSourcePath = os.pwd /  "dependencies" / "treadle"
  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
}
object mychisel3 extends dependencies.chisel3.build.chisel3CrossModule(v.scala) {
  override def millSourcePath = os.pwd / "dependencies" / "chisel3"
  def firrtlModule: Option[PublishModule] = Some(myfirrtl)
  def treadleModule: Option[PublishModule] = Some(mytreadle)
  def chiseltestModule: Option[PublishModule] = Some(mychiseltest)
}
object mychiseltest extends dependencies.chiseltest.build.chiseltestCrossModule(v.scala) {
  override def millSourcePath = os.pwd /  "dependencies" / "chiseltest"
  def chisel3Module: Option[PublishModule] = Some(mychisel3)
  def treadleModule: Option[PublishModule] = Some(mytreadle)
}
object vector extends common.VectorModule with ScalafmtModule { m =>
  def millSourcePath = os.pwd / "v"
  def scalaVersion = T { v.scala }
  def chisel3Module = Some(mychisel3)
  def chisel3PluginJar = T { Some(mychisel3.plugin.jar()) }
  def chiseltestModule = Some(mychiseltest)
  def utest: T[Dep] = v.utest

  object tests extends Tests with Utest with ScalafmtModule {
    override def scalacPluginIvyDeps = T { m.scalacPluginIvyDeps() }
    override def scalacOptions = T { m.scalacOptions() }
    override def moduleDeps = super.moduleDeps ++ chiseltestModule
    override def ivyDeps = T {
      super.ivyDeps() ++
        Agg(utest()) ++
        chiseltestIvyDep()
    }
  }

}
