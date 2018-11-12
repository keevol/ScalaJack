package co.blocke.scalajack

// Scala classes should mix this in
trait SJCapture {
  var captured: Option[IRAndOps] = None
}

// Java classes should inherit this!
class SJCaptureWrapper extends SJCapture