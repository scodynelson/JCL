package jcl.compiler.sa.analyzer.body;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.array.StringStructImpl;

public class BodyProcessingResult {

	private final List<LispStruct> declares;

	private final StringStructImpl docString;

	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final List<LispStruct> declares, final StringStructImpl docString, final List<LispStruct> bodyForms) {
		this.declares = declares;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public List<LispStruct> getDeclares() {
		return declares;
	}

	public StringStructImpl getDocString() {
		return docString;
	}

	public List<LispStruct> getBodyForms() {
		return bodyForms;
	}
}
