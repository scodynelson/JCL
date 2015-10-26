package jcl.compiler.sa.analyzer.body;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;

public class BodyProcessingResult {

	private final List<LispStruct> declares;

	private final StringStruct docString;

	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final List<LispStruct> declares, final StringStruct docString, final List<LispStruct> bodyForms) {
		this.declares = declares;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public List<LispStruct> getDeclares() {
		return declares;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<LispStruct> getBodyForms() {
		return bodyForms;
	}
}
