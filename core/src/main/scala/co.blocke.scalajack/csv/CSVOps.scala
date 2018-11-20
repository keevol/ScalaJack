package co.blocke.scalajack
package csv

import org.json4s.JValue
import json.Json4sOpsBase

object CSVOps extends Ops[JValue, String] with Json4sOpsBase with CSVDeserializer[JValue] with CSVSerializer[JValue]
