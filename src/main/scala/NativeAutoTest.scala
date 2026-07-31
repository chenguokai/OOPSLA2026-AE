import scala.sys.process._
import scala.util.Try
import scala.collection.mutable



object NativeTest:

  /** Compute the mean and standard deviation for a sequence of numbers. */
  def computeStats(data: Seq[Double]): (Double, Double) =
    val mean = data.sum / data.size
    val variance = data.map(x => math.pow(x - mean, 2)).sum / data.size
    val stddev = math.sqrt(variance)
    (mean, stddev)

  /** Run the external program /tmp/Ga.elf and return its output as a Vector[Double].
   *
   * Assumes that the program prints one or more whitespace‐separated numbers.
   */
  def runGa(): Option[Vector[Double]] =
    try {
      // Run the external program and capture its raw output.
      val rawOutput = Process("/tmp/Ga.elf").!!.trim

      // Filter out any line matching "Thread bound to CPU X"
      // This splits the output into lines, removes unwanted lines, and then rejoins them.
      val filteredOutput = rawOutput
        .linesIterator
        .filterNot(line => line.matches("Thread bound to CPU \\d+"))
        .mkString(" ")

      // Split the filtered output on whitespace and parse into Doubles.
      val numbers = filteredOutput.split("\\s+").toVector.map(_.toDouble)
      Some(numbers)
    } catch {
      case e: Exception =>
        System.err.println(s"Error running /tmp/Ga.elf: ${e.getMessage}")
        None
    }


  /** Runs the complete test suite.
   *
   * @param testConfig
   *   a function that accepts a configuration parameter B (an Int) and adjusts the test environment accordingly.
   * @param maxB
   *   the maximum configuration value (the test range will be 0 to maxB).
   * @param runsPerB
   *   how many times to run /tmp/Ga.elf for each configuration B.
   */
  def runTestSuite(testConfig: String, maxB: Int, runsPerB: Int): Unit =
    println(s"Starting tests for B in range 0 to $maxB, with $runsPerB runs per B.")

    // A map to hold, for each configuration B, the list of outputs (each run's output is a Vector[Double]).
    val results = mutable.Map.empty[Int, List[Vector[Double]]]

    // For each configuration value B, call the configuration function and then run /tmp/Ga.elf several times.
    for B <- 0 to maxB do
      runInNewProcessWithParam(testConfig, B.toString)
      val runOutputs = (1 to runsPerB).flatMap { runIndex =>
        println(s"Running /tmp/Ga.elf for B = $B, run #$runIndex")
        runGa() match
          case Some(output) =>
            println(s"Output for B = $B, run #$runIndex: $output")
            Some(output)
          case None =>
            System.err.println(s"Failed to obtain output for B = $B, run #$runIndex")
            None
      }.toList

      if runOutputs.isEmpty then
        System.err.println(s"No successful runs for B = $B, skipping this configuration.")
      else
        // Check that every run for this B produced the same number of output numbers.
        val lengths = runOutputs.map(_.length).toSet
        if lengths.size != 1 then
          System.err.println(s"Error: Inconsistent output lengths for B = $B. Found lengths: $lengths")
        else
          results(B) = runOutputs

    // Compute statistics for each configuration B.
    // For each B, and for each “column” (i.e. each position in the output vector),
    // compute the mean and standard deviation over the runs.
    val statsByB: Map[Int, Vector[(Double, Double)]] = results.map { case (b, runs) =>
      val n = runs.head.length
      val stats: Vector[(Double, Double)] = (0 until n).toVector.map { colIndex =>
        val colValues = runs.map(output => output(colIndex))
        computeStats(colValues)
      }
      (b, stats)
    }.toMap

    // Report an error if the output for a given B is “unstable” (i.e. too high internal variation).
    val internalVarThreshold = 0.1 // 10% coefficient of variation
    for ((b, stats) <- statsByB) do
      stats.zipWithIndex.foreach { case ((mean, stddev), colIndex) =>
        if mean != 0 && (stddev / math.abs(mean)) > internalVarThreshold then
          println(f"Error: For B = $b, column $colIndex has high variation: mean = $mean%.4f, stddev = $stddev%.4f")
      }

    // For each output column, compare the mean values across the different B values.
    // If the relative difference (max minus min, divided by the overall mean) exceeds a significance threshold,
    // we report a “Hit”; otherwise a “Miss”.
    if statsByB.nonEmpty then
      val numColumns = statsByB.head._2.size
      for colIndex <- 0 until numColumns do
        // Map from configuration B to the mean for this column.
        val meansForB: Map[Int, Double] = statsByB.map { case (b, stats) =>
          (b, stats(colIndex)._1)
        }
        val meanValues = meansForB.values.toVector
        val overallMean = meanValues.sum / meanValues.size
        val minMean = meanValues.min
        val maxMean = meanValues.max
        val diff = maxMean - minMean
        val relativeDiff = if overallMean != 0 then diff / math.abs(overallMean) else diff
        val significanceThreshold = 0.05 // 5% relative difference
        if relativeDiff > significanceThreshold then
          println(s"Hit: Column $colIndex shows significant variation across B values. " +
            s"(min mean: $minMean, max mean: $maxMean, relative diff: $relativeDiff). " +
            s"Detailed means: $meansForB")
        else
          println(s"Miss: Column $colIndex shows non-significant variation across B values. " +
            s"(min mean: $minMean, max mean: $maxMean, relative diff: $relativeDiff). " +
            s"Detailed means: $meansForB")
    else
      println("No valid results to compare across configurations.")

  /** A dummy configuration function that simply prints the value of B.
   *
   * In a real scenario you might adjust environment variables, command‐line options,
   * or other parameters for /tmp/Ga.elf here.
   */
  def dummyConfig(B: Int): Unit =
    println(s"Configuring test environment for B = $B")
  // Insert any real configuration logic here.

  /** The main method.
   *
   * Usage:
   *   sbt run <maxB> [runsPerB]
   *
   * For example, "sbt run 5 10" would test B = 0,1,...,5 with 10 runs each.
   */
  def run(args: Array[String] = Array("1"), func: String): Unit =
    // If no arguments were provided, use the default value.
    val actualArgs = if args.isEmpty then Array("1") else args

    // Now you can check the length of actualArgs as needed.
    if actualArgs.length < 1 then
      System.err.println("Usage: GaTest <maxB> [runsPerB]")
      sys.exit(1)

    // Parse your arguments.
    val maxB = try actualArgs(0).toInt
    catch
      case _: NumberFormatException =>
        System.err.println("Invalid maxB value")
        sys.exit(1)
    val runsPerB =
      if actualArgs.length >= 2 then
        try actualArgs(1).toInt
        catch
          case _: NumberFormatException =>
            System.err.println("Invalid runsPerB value")
            sys.exit(1)
      else 10

    // Run the test suite using our dummy configuration function.
    runTestSuite(func, maxB, runsPerB)
