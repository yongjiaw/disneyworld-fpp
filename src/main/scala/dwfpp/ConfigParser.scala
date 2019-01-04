package dwfpp

import java.io.File

import scala.util.matching.Regex

import com.typesafe.config.{Config, ConfigFactory}

import org.datacrafts.noschema.NoSchema
import org.datacrafts.noschema.json.{JsonOperation, JsonOperationDsl}

abstract class ConfigParser[T: NoSchema] extends JsonOperationDsl {

  def parseConfig(config: Config): T = {
    val jsonOp = jsonOperationOf[T]()
    val json = JsonOperation.objectMapper.writeValueAsString(config.root().unwrapped())
    jsonOp.fromJson(json)
    // schemaByShapeless[T].operation().marshal(config.root().unwrapped().asScala)
  }
}

case class FastPassGrabberConfig(
  chromeDriverLocation: String,
  fastPassConfig: FastPassConfig
)

case class FastPassConfig(
  initialPageUrl: String,
  landingPageSignature: String,
  timeParsingRegex: String,
  selection: ExperienceSelectionConfig
) {
  val timeRegex = timeParsingRegex.r
}

case class ExperienceSelectionConfig(
  namePatterns: Seq[String],
  timeSlots: Seq[TimeSlot],
  pollingInterval: Option[Long]
)

case class TimeSlot(
  start: HourAndMinute,
  end: HourAndMinute
) {
  def contains(time: HourAndMinute): Boolean = {
    (start < time || start == time) && (time < end || time == end)
  }
}

case class HourAndMinute(hour: Int, min: Int) {
  def < (other: HourAndMinute): Boolean = {
    hour < other.hour || hour == other.hour && min < other.min
  }

}

object HourAndMinute {
  val HourAndMinutePattern = """^(?i)\s*([0-9]+)\:([0-9]+)([A|P]M)\s*$""".r
  def fromString(value: String, regex: Regex): HourAndMinute = {
    value match {
      case regex(hour, min, part) =>
        HourAndMinute(
          if (part.toUpperCase() == "PM" && hour.toInt < 12) hour.toInt + 12 else hour.toInt,
          min.toInt)
      case _ => throw new Exception(s"${value} does not match pattern ${HourAndMinutePattern}")
    }
  }
}

object TestLoader extends ConfigParser[FastPassGrabberConfig] {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.parseFile(new File("./application.conf")).resolve()
    println(parseConfig(config))
  }
}
