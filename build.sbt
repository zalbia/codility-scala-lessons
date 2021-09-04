name := "codility-scala"

version := "1.0"

scalaVersion := "2.12.14" // Codility runs on 2.12

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
