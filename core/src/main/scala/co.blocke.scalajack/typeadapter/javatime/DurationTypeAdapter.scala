package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration

object DurationTypeAdapter extends TypeAdapter.=:=[Duration] {
  override val deserializer: Deserializer[Duration] = new DurationDeserializer
  override val serializer: Serializer[Duration] = new DurationSerializer
}
