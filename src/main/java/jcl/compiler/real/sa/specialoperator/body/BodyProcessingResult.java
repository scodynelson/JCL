package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.sa.element.declaration.DeclareElement;

import java.util.List;

public class BodyProcessingResult {

	private final DeclareElement declareElement;
	private final StringStruct docString;
	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final DeclareElement declareElement, final StringStruct docString, final List<LispStruct> bodyForms) {
		this.declareElement = declareElement;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public DeclareElement getDeclareElement() {
		return declareElement;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<LispStruct> getBodyForms() {
		return bodyForms;
	}
}
