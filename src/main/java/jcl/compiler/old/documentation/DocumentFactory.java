package jcl.compiler.old.documentation;

import org.w3c.dom.Document;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

public class DocumentFactory {

	private DocumentBuilderFactory docBuildFactory;
	private DocumentBuilder parser;

	public DocumentFactory() {
		try {
			docBuildFactory = DocumentBuilderFactory.newInstance();
			parser = docBuildFactory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			System.out.println(e);
		}
	}

	public Document newInstance() {
		return parser.newDocument();
	}
}
