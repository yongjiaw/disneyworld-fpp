package dwfpp

import java.io.File
import java.util.LongSummaryStatistics

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

import com.typesafe.config.ConfigFactory
import org.openqa.selenium.{By, WebElement}
import org.openqa.selenium.chrome.ChromeDriver

import org.datacrafts.logging.Slf4jLogging

object FastPassPlusGrabber extends ConfigParser[FastPassGrabberConfig] with Slf4jLogging.Default {

  import scala.collection.JavaConverters._

  private var _config: FastPassGrabberConfig = null
  def main(args: Array[String]): Unit = {

    val fileName = if (args.isEmpty) "./application.conf" else args(0)
    val config = ConfigFactory.parseFile(new File(fileName)).resolve()
    _config = parseConfig(config)

    System.setProperty(
      "webdriver.chrome.driver",
      _config.chromeDriverLocation
    )
    val driver = new ChromeDriver()

    Try {
      driver.get(_config.fastPassConfig.initialPageUrl)
      blockUntilSelectionPage(driver)
      logInfo("landed on selection page, everything else is automated now")
      selectTime(driver)
    } match {
      case Success(_) =>
        StdIn.readLine("Press RETURN to close the browser window...")
        driver.quit()
        logInfo(s"successfully closed the browser")
      case Failure(f) =>
        driver.quit()
        logInfo(s"successfully closed the browser")
        throw f
    }

  }

  private val experienceHitStats = collection.mutable.Map.empty[String, LongSummaryStatistics]
  private def printStats(): Unit = {
    experienceHitStats.toSeq.sortBy(_._1).foreach {
      case (key, stats) =>
        logInfo(s"${key}: foundTime[first=${stats.getMin}, last=${stats.getMax}], count=${stats.getCount}")
    }
  }

  case class ExperienceHit(
    uniqueKey: String,
    timeButton: WebElement,
    matched: Boolean
  )

  private def selectTime(driver: ChromeDriver): Unit = {

    def tryConfirm = Try {
      val confirm = driver.findElements(By.cssSelector("div[class~=confirm]")).asScala.head
      confirm.click()
    }

    while(
      {
        val availableTimes = findAvailableTimes(driver)

        // name pattern has higher priority over time, this is for simultaneously searching multiple rides
        // if need to fit a specific time slot, multiple grabbers can run for each slot
        _config.fastPassConfig.selection.timeSlots

        var namePatternMatched = false
        val hits =
        for (
          // there will be multiple cross hits between regex and name
          regex <- _config.fastPassConfig.selection.namePatterns;
          (experienceString, times) <- availableTimes
          if {
            experienceString.matches(regex) && {
              logInfo(s"found matching experience: ${experienceString}, availableTimes=${times.map(_.getText)}")
              namePatternMatched = true
              true
            }
          };
          // there will be multiple cross hits for time slots and time available
          timeSlot <- _config.fastPassConfig.selection.timeSlots;
          time <- times
        ) yield {
          val timeMatched = timeSlot.contains(HourAndMinute.fromString(time.getText, _config.fastPassConfig.timeRegex))
          if (timeMatched) {
            logInfo(s"found matches for pattern=${regex}, timeSlot=${timeSlot}: ${experienceString} at ${time.getText}")
          }

          ExperienceHit(
            s"${experienceString} at ${time.getText}",
            time,
            timeMatched
          )
        }

        if (!namePatternMatched) {
          logWarning(s"name patterns ${_config.fastPassConfig.selection.namePatterns} does not match any item: ${availableTimes.keySet}")
        }

        hits.map(_.uniqueKey).toSet.foreach[Unit](
          hit => experienceHitStats
            .getOrElseUpdate(hit, new LongSummaryStatistics)
            .accept(System.currentTimeMillis)
        )

        val success =
        hits.collectFirst{
          case hit if hit.matched =>
            hit.timeButton.click()
            while (
              tryConfirm match {
                case Success(_) =>
                  logInfo(s"confirm success: ${hit.uniqueKey}")
                  false
                case Failure(f) =>
                  logInfo(s"failed to click confirm, retry after sleep: ${f.getMessage}")
                  true
              }
            ) {
              Thread.sleep(1000)
            }
            true
        }.getOrElse(false)

        !success
      }
    ) {
      printStats()
      Thread.sleep(
        _config.fastPassConfig.selection.pollingInterval.getOrElse(5000) // sleep 5 seconds by default
      )
      driver.navigate().refresh()
    }
  }

  private def findAvailableTimes(driver: ChromeDriver): Map[String, Seq[WebElement]] = {

    var experiences = driver.findElements(By.cssSelector("div[data-experience-id]")).asScala

    while (experiences.size == 0 || experiences.map(_.getText).mkString("") == "" ) {
      val errorElements = driver.findElements(By.cssSelector("div[id=serviceErrorPage]"))
      if (errorElements.size() > 0) {
        logWarning(s"error page encountered, auto refresh: ${errorElements.asScala.map(_.getText).mkString("")}")
        Thread.sleep(5000)
        driver.navigate().refresh()
      } else {
        logInfo(s"waiting for experiences list to load, make sure the browser window is visible")
        Thread.sleep(1000)
        experiences = driver.findElements(By.cssSelector("div[data-experience-id]")).asScala
      }

    }
    logInfo(s"experiences list loaded")
    (for (
      experience <- experiences
    ) yield {
      experience.findElements(By.cssSelector("div[class^=name]")).asScala.map(_.getText).mkString(",") ->
        experience.findElements(By.cssSelector("div[class^=availableTime]")).asScala
    }).toMap
  }

  private def waitWhile(
    condition: => Boolean,
    message: => String,
    timeOutMs: Long = -1
  ): Unit = {
    val initialTime = System.currentTimeMillis()
    var lastPrintTime = 0L
    while(!condition) {
      if (System.currentTimeMillis() - lastPrintTime > 10000) {
        logInfo(s"waited ${System.currentTimeMillis() - initialTime}ms: ${message}")
        lastPrintTime = System.currentTimeMillis()
      }
      Thread.sleep(1000)
      val waitedMs = System.currentTimeMillis() - initialTime
      if (timeOutMs > 0 && waitedMs > timeOutMs) {
        throw new Exception(s"timeout after ${waitedMs}ms for: ${message}")
      }
    }
    logInfo(s"success: ${message}")
  }

  private def blockUntilSelectionPage(driver: ChromeDriver): Unit = {
    val pageSelector = _config.fastPassConfig.landingPageSignature
    def elements = driver
      .findElements(By.cssSelector(pageSelector))

    waitWhile(
      condition = Try {
        elements.size() > 0 && {
          logInfo(s"selectExperiencePage text: ${elements.get(0).getText}")
          elements.get(0).getText != ""
        }
      }.toOption.getOrElse(false),

      message = {
        s"required manual steps before automation: go to browser, login, select party, select date, select park, " +
          s"click desirable day part (Morning, Afternoon, Evening), " +
          s"(signature: ${pageSelector}, found: ${elements.size()})"
      }
    )
  }

}
