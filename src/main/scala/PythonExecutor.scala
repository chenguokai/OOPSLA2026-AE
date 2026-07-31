package HT.Executors

import scala.sys.process.*
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.Map

object PythonExecutor:
  def executePythonCode(pythonCode: String): Either[String, String] =
    try
      // Create a temporary file with .py extension
      val tmpFile = Files.createTempFile("temp_python_", ".py")

      // Write the Python code to the temporary file
      val writer = PrintWriter(tmpFile.toFile)
      try
        writer.write(pythonCode)
      finally
        writer.close()

      // Make the file executable
      tmpFile.toFile.setExecutable(true)

      // Execute the Python script and capture output
      val result = Try {
        val output = Seq("python3", tmpFile.toString).!!
        output.trim
      }

      // Clean up the temporary file
      //Files.delete(tmpFile)

      result match
        case Success(output) => Right(output)
        case Failure(error) => Left(s"Error executing Python code: ${error.getMessage}")

    catch
      case e: Exception => Left(s"Error: ${e.getMessage}")

def pyStr2Result(pyStr: String): Map[String, Long] = {
  // output format: KEY = VALUE
  val results = PythonExecutor.executePythonCode(pyStr) match {
    case Right(output) => output.split("\n")
    case Left(error) => throw new Exception(s"Error: $error")
  }
  // return empty map if no results
  if (results(0) == "") {
    return collection.mutable.Map.empty
  }
  // convert to map
  results.map { line =>
    val Array(key, value) = line.split("=").map(_.trim)
    key -> value.toString.toLong
  }.toMap.to(collection.mutable.Map)
}

// Example usage
@main def main() =
  val pythonCode = """
                     |print("Hello from Python!")
                     |x = 10 + 20
                     |print(f"Result: {x}")
    """.stripMargin

  PythonExecutor.executePythonCode(pythonCode) match
    case Right(output) => println(s"Output:\n$output")
    case Left(error) => println(s"Error: $error")