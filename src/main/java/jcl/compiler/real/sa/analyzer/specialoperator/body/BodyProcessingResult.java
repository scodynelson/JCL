package jcl.compiler.real.sa.analyzer.specialoperator.body;

import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.StringElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;

import java.util.List;

public class BodyProcessingResult {

	private final DeclareElement declareElement;

	private final StringElement docString;

	private final List<SimpleElement> bodyForms;

	public BodyProcessingResult(final DeclareElement declareElement, final StringElement docString, final List<SimpleElement> bodyForms) {
		this.declareElement = declareElement;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public DeclareElement getDeclareElement() {
		return declareElement;
	}

	public StringElement getDocString() {
		return docString;
	}

	public List<SimpleElement> getBodyForms() {
		return bodyForms;
	}
}
