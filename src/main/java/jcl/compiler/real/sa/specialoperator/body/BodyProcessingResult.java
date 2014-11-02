package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.LispType;
import jcl.structs.arrays.StringStruct;

import java.util.List;

public class BodyProcessingResult implements LispStruct {

	private final List<LispStruct> declarations;
	private final StringStruct docString;
	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final List<LispStruct> declarations, final StringStruct docString, final List<LispStruct> bodyForms) {
		this.declarations = declarations;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public List<LispStruct> getDeclarations() {
		return declarations;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<LispStruct> getBodyForms() {
		return bodyForms;
	}

	@Override
	public LispType getType() {
		return null;
	}

	@Override
	public String printStruct() {
		return null;
	}
}
