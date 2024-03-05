name := "ASG"
version := "1.0"

scalaVersion := "2.12.16"

val spinalVersion = "1.8.0b"

// Added the spinal libraries
libraryDependencies ++= Seq(
  "cc.redberry" %% "rings.scaladsl" % "2.5.7",
  "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
  "com.github.spinalhdl" %% "spinalhdl-lib"  % spinalVersion,
  "com.github.spinalhdl" %% "spinalhdl-sim"  % spinalVersion,
  compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
)

fork := true

