package bildaj.codegen

import bildaj._
import bildaj.jsonld._
import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.vocabulary.{RDF, RDFS, OWL, XSD}
import java.net.URI
import org.apache.jena.riot.RDFDataMgr
import scala.collection.JavaConversions._

object CodeGen {
  import JsonLDSchema._

  sealed trait CodeGenType

  class CodeGenClass(val uri : URI) extends CodeGenType {
    var fields = List[CodeGenField]()
  }

  class CodeGenValue(val _type : String) extends CodeGenType 

  object CodeGenAnyURI extends CodeGenType

  object CodeGenAny extends CodeGenType

  case class CodeGenField(name : String, uri : URI, range : CodeGenType, functional : Boolean = false)

  private def mappingToURI(m : TermMapping) = m match {
    case TypedMapping(uri,_,_) => Some(uri)
    case LangStringMapping(uri,_) => Some(uri)
    case LangContainerMapping(uri) => Some(uri)
    case ListMapping(uri,_) => Some(uri)
    case AliasMapping(_) => None
    case IndexMapping(uri,_) => Some(uri)
  }


  def buildModelForSchema(schema : JsonLDSchema) : Model = {
    def removeFrag(u : URI) = new URI(u.getScheme(), u.getSchemeSpecificPart(), null)
    var loadedURIs = collection.mutable.Set[URI]()
    val model = ModelFactory.createDefaultModel()
    def loadURI(u : URI) = {
      val fragless = removeFrag(u)
      if(!loadedURIs.contains(fragless)) {
        // TODO: Content Negotiation & Format checking
        RDFDataMgr.read(model, fragless.toString)
        loadedURIs += fragless
      }
    }

    schema.base match {
      case Some(base) => loadURI(base)
      case None => // no-op
    }

    for(mapping <- schema.mappings.values) {
      mappingToURI(mapping).map(loadURI(_))
    }

    return model
  }

  private type ProcessedModel = Map[URI,(Option[URI],Option[URI], Boolean, Boolean, List[URI])]

  private def processModel(schema : JsonLDSchema, model : Model) 
    : ProcessedModel = {
    for((key, mapping) <- schema.mappings if !mapping.isInstanceOf[AliasMapping]) yield {
      val uri = mappingToURI(mapping).get
      val stats = model.listStatements(model.createResource(uri.toString), null, null)
      var range : Option[URI] = None
      var domain : Option[URI] = None
      var functional = false
      var inverseFunctional = false
      var superclasses : List[URI] = Nil
      for(stat <- stats) {
        if(stat.getPredicate() == RDFS.range) {
          range = Some(new URI(stat.getObject().toString()))
        } else if(stat.getPredicate() ==  RDFS.domain) {
          domain = Some(new URI(stat.getObject().toString()))
        } else if(stat.getPredicate() == RDF.`type` && stat.getObject() == OWL.FunctionalProperty) {
          functional = true
        } else if(stat.getPredicate() == RDF.`type` && stat.getObject() == OWL.InverseFunctionalProperty) {
          inverseFunctional = true
        } else if(stat.getPredicate() == RDFS.subClassOf) {
          superclasses ::= new URI(stat.getObject().toString())
        }
      }
      uri -> (domain, range, functional, inverseFunctional, superclasses)
    }
  }
    
  def type2type(_type : Either[URI,Boolean], range : Option[URI], 
    schema : JsonLDSchema, model : ProcessedModel, 
    clazzes : collection.mutable.Map[URI, CodeGenClass]) : CodeGenType = _type match {
      case Left(uri) => {
        if(uri.toString.startsWith(XSD.getURI())) {
          new CodeGenValue(uri.getFragment)
        } else {
          throw new RuntimeException("Unsupported datatype " + uri)
        }
      }
      case Right(true) => 
        range match {
          case Some(uri) => buildClass(schema, model, uri, clazzes)
          case None => range match {
            case Some(r) => buildClass(schema, model, r, clazzes)
            case None => CodeGenAnyURI
          }
      }
      case Right(false) =>
        CodeGenAny
  }

   def buildClass(schema : JsonLDSchema, rdfModel : ProcessedModel, clazz : URI, clazzes :
    collection.mutable.Map[URI, CodeGenClass])  : CodeGenClass = {
      clazzes.get(clazz) match {
        case Some(c) => c
        case None =>
          val c = new CodeGenClass(clazz)
          clazzes.put(clazz, c)
          for((name, mapping) <- schema.mappings) {
            mappingToURI(mapping) match {
              case Some(u) => rdfModel.get(u) match {
                case Some((domain,range,functional,inverseFunctional,superClasses)) => {
                  if(!mapping.reverse && domain != None && domain.get == clazz) {    
                    val r = type2type(mapping._type, range, schema, rdfModel, clazzes)
                    c.fields ::= new CodeGenField(name, u, r, functional)
                  } else if(mapping.reverse && range != None && range.get == clazz) {
                    val r = type2type(mapping._type, domain, schema, rdfModel, clazzes)
                    c.fields ::= new CodeGenField(name, u, r, inverseFunctional)
                  }
                }
                case None => throw new RuntimeException("should be unreachable as processModel constructs from schema.mappings")
              }
              case None => // no op
            }
          }
          c
      }
    }
  
  def buildCodeGenModel(schema : JsonLDSchema, rootClass : URI) : CodeGenClass = {
    val clazzes = collection.mutable.Map[URI, CodeGenClass]()

    val rdfModel = buildModelForSchema(schema)

    val processedModel = processModel(schema, rdfModel)

    return buildClass(schema, processedModel, rootClass, clazzes)
  }

}
