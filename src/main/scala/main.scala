import java.io._
import scala.io.{Source, StdIn}
import scala.util.{Try, Success, Failure}


// DATA MODEL

case class EnergyData(
                       source: String,
                       date: String, // DD/MM/YYYY
                       hour: Int,
                       energy: Double
                     )


// FILE SERVICE

object FileService {

  def write(data: List[EnergyData], filename: String): Unit = {
    val writer = new PrintWriter(new File(filename))
    writer.println("source,date,hour,energy")

    data.foreach(d =>
      writer.println(s"${d.source},${d.date},${d.hour},${d.energy}")
    )

    writer.close()
  }

  def read(filename: String): List[EnergyData] = {
    Try {
      val source = Source.fromFile(filename)

      val data = source.getLines().drop(1).toList.map { line =>
        val p = line.split(",")
        EnergyData(p(0), p(1), p(2).toInt, p(3).toDouble)
      }

      source.close()
      data

    } match {
      case Success(data) => data
      case Failure(_) =>
        println("Error reading file.")
        List()
    }
  }
}


// DATA SERVICE

object DataService {

  def filterByDate(data: List[EnergyData], date: String) =
    data.filter(_.date == date)

  def filterByHour(data: List[EnergyData], hour: Int) =
    data.filter(_.hour == hour)

  def filterByMonth(data: List[EnergyData], month: String) =
    data.filter { d =>
      val parts = d.date.split("/")
      parts.length == 3 && parts(1) == month
    }

  def filterByWeek(data: List[EnergyData], start: String, end: String) =
    data.filter(d => d.date >= start && d.date <= end)

  def sortByEnergy(data: List[EnergyData]) =
    data.sortBy(_.energy)

  def search(data: List[EnergyData], key: String) =
    data.filter(d =>
      d.source.toLowerCase.contains(key.toLowerCase)
    )
}


// ANALYSIS

object Analysis {

  def mean(v: List[Double]) = if (v.isEmpty) 0 else v.sum / v.length

  def median(v: List[Double]) = {
    val s = v.sorted
    if (s.length % 2 == 0)
      (s(s.length / 2 - 1) + s(s.length / 2)) / 2
    else s(s.length / 2)
  }

  def mode(v: List[Double]) =
    v.groupBy(identity).maxBy(_._2.size)._1

  def range(v: List[Double]) = v.max - v.min

  def midrange(v: List[Double]) = (v.max + v.min) / 2

  def analyze(data: List[EnergyData]): Unit = {
    val v = data.map(_.energy)

    if (v.isEmpty) {
      println("No data available for analysis.")
    } else {
      println(s"Mean: ${mean(v)}")
      println(s"Median: ${median(v)}")
      println(s"Mode: ${mode(v)}")
      println(s"Range: ${range(v)}")
      println(s"Midrange: ${midrange(v)}")
    }
  }
}


// MONITORING

object Monitoring {
  def detectLow(data: List[EnergyData]) =
    data.filter(_.energy < 25)
}

// SAMPLE DATA

object Sample {
  def data = List(
    EnergyData("Solar", "01/05/2026", 10, 50),
    EnergyData("Wind", "01/05/2026", 11, 30),
    EnergyData("Hydro", "02/05/2026", 12, 70),
    EnergyData("Solar", "03/05/2026", 10, 20)
  )
}


// MAIN APPLICATION

object REPS extends App {

  var data = Sample.data

  def menu(): Unit = {
    println("\n===== REPS MENU =====")
    println("1. View Data")
    println("2. Filter Data")
    println("3. Sort Data")
    println("4. Search")
    println("5. Analyze")
    println("6. Detect Issues")
    println("7. Save Data to File")
    println("8. Load Data from File")
    println("9. Exit")
  }

  def filterMenu(): Unit = {
    println("\n--- Filter Options ---")
    println("1. Daily")
    println("2. Hourly")
    println("3. Weekly")
    println("4. Monthly")
  }

  def loop(): Unit = {

    menu()
    val c = StdIn.readLine("Choose: ")

    c match {

      case "1" =>
        data.foreach(println)

      case "2" =>
        filterMenu()
        val f = StdIn.readLine("Choose filter: ")

        f match {
          case "1" =>
            val d = StdIn.readLine("Enter date (DD/MM/YYYY): ")
            DataService.filterByDate(data, d).foreach(println)

          case "2" =>
            val input = StdIn.readLine("Enter hour: ")

            Try(input.toInt) match {
              case Success(h) =>
                DataService.filterByHour(data, h).foreach(println)
              case Failure(_) =>
                println("Invalid hour input.")
            }


          case "3" =>
            val start = StdIn.readLine("Start date (DD/MM/YYYY): ")
            val end = StdIn.readLine("End date (DD/MM/YYYY): ")
            DataService.filterByWeek(data, start, end).foreach(println)

          case "4" =>
            val m = StdIn.readLine("Enter month (MM): ")
            DataService.filterByMonth(data, m).foreach(println)

          case _ =>
            println("Invalid filter option.")
        }

      case "3" =>
        data = DataService.sortByEnergy(data)
        println("Data sorted.")

      case "4" =>
        val k = StdIn.readLine("Search: ")
        DataService.search(data, k).foreach(println)

      case "5" =>
        Analysis.analyze(data)

      case "6" =>
        val issues = Monitoring.detectLow(data)

        if (issues.isEmpty) println("No issues detected.")
        else {
          println("Low energy alerts:")
          issues.foreach(println)
        }

      case "7" =>
        val filename = StdIn.readLine("Enter file name: ")
        FileService.write(data, filename)
        println("Data saved.")

      case "8" =>
        val filename = StdIn.readLine("Enter file name: ")
        val loaded = FileService.read(filename)

        if (loaded.isEmpty) {
          println("No data loaded.")
        } else {
          data = loaded
          println("Data loaded.")
        }

      case "9" =>
        sys.exit()



      case _ =>
        println("Invalid option.")
    }

    loop()
  }

  loop()
}


