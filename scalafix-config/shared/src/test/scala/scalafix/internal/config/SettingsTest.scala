package scalafix.internal.config

import org.scalatest.FunSuite
import scalafix.config._
import com.typesafe.config.ConfigFactory
import io.circe.HCursor
import io.circe.Json

case class AllTheAnnotations(
    @SettingDescription("descriptioon")
    @ExampleValue("value")
    @ExampleValue("value2")
    @ExtraSettingName("extraName")
    @ExtraSettingName("extraName2")
    @DeprecatedSettingName("deprecatedName", "Use x instead", "2.0")
    @DeprecatedSettingName("deprecatedName2", "Use y instead", "3.0")
    @SinceVersion("2.1")
    @SettingDescription("Description")
    @DeprecatedSetting("Use newFeature instead", "2.1")
    setting: Int = 2,
    setting2: String = "default"
)

object AllTheAnnotations {
  implicit lazy val fields: Fields[AllTheAnnotations] =
    Macros.deriveFields[AllTheAnnotations]
}

class ConfDecoderSuite extends FunSuite {
  test("ConfDecoder[T]") {
    val decoder = ConfigDecoder.derive[AllTheAnnotations](AllTheAnnotations())
    val config = HoconConfig.fromString("""
                                          |setting2=blah
                                          |setting=5423
                                          |""".stripMargin)
    val obtained = decoder.decode(config)
    println(obtained)
  }
}

class SettingsSuite extends FunSuite {
  test("Settings[T]") {
    val List(s1, s2) = Settings[AllTheAnnotations].settings
    assert(s1.name == "setting")
    assert(s1.field.defaultValue.get.value == 2)
    assert(
      s1.extraNames == List(
        "extraName",
        "extraName2"
      )
    )
    assert(
      s1.deprecatedNames ==
        List(
          DeprecatedSettingName("deprecatedName", "Use x instead", "2.0"),
          DeprecatedSettingName("deprecatedName2", "Use y instead", "3.0"))
    )
    assert(
      s1.exampleValues ==
        List("value", "value2")
    )
    assert(s1.description.contains("descriptioon"))
    assert(s1.sinceVersion.contains("2.1"))
    assert(
      s1.deprecated.contains(DeprecatedSetting("Use newFeature instead", "2.1"))
    )

    assert(s2.name == "setting2")
    assert(s2.field.defaultValue.get.value == "default")
    assert(s2.annotations.isEmpty)
  }
}
