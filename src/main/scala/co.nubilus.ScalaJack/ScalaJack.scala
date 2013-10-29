package co.nubilus.scalajack

/**
 * Assumptions:
 *   -- Case classes only
 *   -- Options of value None are removed from generated JSON
 *   -- Default parameters are not supported at this time
 *   -- Simple types supported:
 *         Int, Boolean, Long, String, Enumeration.Value
 *   -- Collections/"containers" supported:
 *         List (mutable (ListBuffer) and immutable)
 *         Map  (mutable and immutable)
 *         Option
 *   -- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)
 *   
 * Useful references:
 * 		http://stackoverflow.com/questions/17006271/deep-access-of-fields-in-scala-using-runtime-reflection
 */

import com.fasterxml.jackson.core.JsonFactory

object ScalaJack {
	type JSON = String
	
	val jsFactory = new JsonFactory();
	
	def render[T]( target:T )(implicit m:Manifest[T]) : JSON = {
		val sb = new StringBuilder
		Analyzer(target.getClass.getName).render(sb, target, None)
		sb.toString
	}

	def renderList[T]( target:List[T] )(implicit m:Manifest[T]) : JSON = {
		val sb = new StringBuilder
		if( target.size == 0 ) sb.append("[]")
		else ListField( "", null, Analyzer(m.runtimeClass.getName) ).render(sb, target, None)
		sb.toString
	}

	def read[T]( js:JSON )(implicit m:Manifest[T]) : T = {
		val jp = jsFactory.createParser(js)
		jp.nextToken
		Analyzer(m.runtimeClass.getName) match {
			case t:TraitField => t.readClass(jp).asInstanceOf[T]
			case c:CaseClassField => c.readClass(jp).asInstanceOf[T]
		}
	}

	def readList[T]( js:JSON )(implicit m:Manifest[T]) : List[T] = {
		val jp = jsFactory.createParser(js)
		jp.nextToken
		ListField( "", null, Analyzer(m.runtimeClass.getName) ).readValue(jp).asInstanceOf[List[T]]
	}

	// Magically create an instance of a case class given a map of name->value parameters.
	// (Reflects on the apply method of the case class' companion object.)
	def poof[T]( data:Map[String,Any] )(implicit m:Manifest[T]) : T = poof( m.runtimeClass.getName, data ).asInstanceOf[T]
		
	private[scalajack] def poof( cname:String, data:Map[String,Any] ) : Any = {
		val classField = Analyzer(cname).asInstanceOf[CaseClassField]
		val args = classField.fields.collect{ case f => data.get(f.name).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
		classField.applyMethod.invoke( classField.caseObj, args:_* )
	}
}
