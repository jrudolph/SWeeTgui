package net.virtualvoid.pdf

import _root_.net.virtualvoid.scala.Tools._
import _root_.com.lowagie.text.pdf._
import _root_.net.virtualvoid.swt.SWTTools._

object IOTools {
	import _root_.java.io._
	
	def hexdumpd(data: Array[Byte]): String = 
		try {
			new String(pipeThrough("hexdump -C", data))
		} catch {
			case e:IOException => "You must have 'hexdump' installed and in the path. Error: "+e.getMessage
		}
	
	def pipeThrough(cmd: String, data: Array[Byte]): Array[Byte] = {
		val baos = new ByteArrayOutputStream
		val proc = Runtime.getRuntime.exec(cmd)
		
		val os = proc.getOutputStream()
		val is = proc.getInputStream()
		os.write(data)
		os.close()
		
		var read = 0
		val buffer = new Array[Byte](100)
		
		do {
			read = is.read(buffer)
			if (read >= 0)
				baos.write(buffer,0,read)
		}while (read >= 0)
			
		baos.toByteArray()
	}
}

object PdfBrowser extends net.virtualvoid.swt.TreeApp[PdfReader] {
	var filename: String = _
	var reader: PdfReader = _
	
	import PdfObject._
	def typeStr(tpe: Int): String = tpe match {
		case ARRAY => "ARRAY"
	    case BOOLEAN => "BOOLEAN"
	    case DICTIONARY => "DICTIONARY"
	    case NAME => "NAME"
	    case NUMBER => "NUMBER"
	    case STRING => "STRING"
	    case INDIRECT => "INDIRECT"
	    case STREAM => "STREAM"
	    case _ => tpe.toString
	}
	def typeStr(obj: PdfObject): String = obj.`type` match {
		case INDIRECT => "INDIRECT => "+typeStr(PdfReader.getPdfObject(obj).`type`)
		case x => typeStr(x)
	}
	
	import net.virtualvoid.swt.Trees._
	
	def asSeq[T](col: java.util.Collection[_]): Seq[T] = col.toArray.toSeq.asInstanceOf[Seq[T]]
	
	def dictionaryEntries(dic: PdfDictionary): Iterable[(String, PdfObject)] =
		asSeq[PdfName](dic.getKeys) map (name => (name.toString, dic.get(name)))
		
	def arrayEntries(arr: PdfArray): Iterable[PdfObject] =
		asSeq[PdfObject](arr.getArrayList())
	
	def pdfObject: ItemInfo[PdfObject] =
		choose {
		case PdfString => string
		case PdfIndirectReference => indirect
		case PdfName => name
		case PdfArray => array
		case PdfNumber => number
		case PRStream => stream
		case PdfDictionary => dictionary
		}
	
	def stream: ItemCreator[PRStream] =
		item[PRStream].|-!("Length: "+_.getRawLength)
					   .|--(dictionary labelled "Dictionary")
					   .contextMenu("Plaintext")(x => textBox(new String(PdfReader.getStreamBytes(x))))
					   .contextMenu("Hexdump")(x => textBox(IOTools.hexdumpd(PdfReader.getStreamBytes(x))))
					   .contextMenu("Hexdump (raw)")(x => textBox(IOTools.hexdumpd(PdfReader.getStreamBytesRaw(x))))
	
	def array: ItemCreator[PdfArray] =
		item[PdfArray].|-*(arrayEntries(_).zipWithIndex) { info =>
			info.as(_._1)(pdfObject).labelled(x => "["+x._2.toString+"]: "+typeStr(x._1))
		}
	
	def indirect: ItemCreator[PdfIndirectReference] =
		item[PdfIndirectReference]//.|-!("<indirect> "+_)
		                          .as(PdfReader.getPdfObject(_))(pdfObject)
	
    def name: ItemCreator[PdfName] = 
    	item[PdfName] |-! ("<name> "+_.toString)
    	
    def number: ItemCreator[PdfNumber] = 
    	item[PdfNumber] |-! (_.toString)
    	
	def string: ItemCreator[PdfString] =
		item[PdfString] |-! ("'"+_.toString+"'")
		
	def dictionary: ItemInfo[PdfDictionary] = 
		item[PdfDictionary].|-*(dictionaryEntries) { info =>
		  	info.as(_._2)(pdfObject).labelled(x => x._1+": "+typeStr(x._2))
		}
	
	def treeCreator: ItemCreator[PdfReader] = 
		item[PdfReader].labelled(filename)
		               .|--(_.getCatalog)(dictionary labelled "Root")
	
    def startObject: PdfReader = reader
    override def title: String = "PDF Structure Viewer: "+filename
		               
	def main(args: Array[String]) {
		if (args.length != 1){
			println("Usage: pdfstructure <filename>")
			exit
		}			
		
		filename = args(0)
		reader = new PdfReader(filename)
		run
	}
}
