package jcl.compiler.real.sa.analyzer.specialoperator.body;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;

public class BodyProcessingResult {

	private final DeclareStruct declareElement;

	private final StringStruct docString;

	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final DeclareStruct declareElement, final StringStruct docString, final List<LispStruct> bodyForms) {
		this.declareElement = declareElement;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public DeclareStruct getDeclareElement() {
		return declareElement;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<LispStruct> getBodyForms() {
		return bodyForms;
	}
}
