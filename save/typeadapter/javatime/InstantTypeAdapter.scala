package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Instant

object InstantTypeAdapter extends TypeAdapter.=:=[Instant] {
  override val deserializer: Deserializer[Instant] = new InstantDeserializer
  override val serializer: Serializer[Instant] = new InstantSerializer
}
