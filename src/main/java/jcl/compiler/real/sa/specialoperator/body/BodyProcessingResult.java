package jcl.compiler.real.sa.specialoperator.body;

import jcl.LispStruct;
import jcl.LispType;
import jcl.arrays.StringStruct;
import jcl.lists.ListStruct;

import java.util.List;

public class BodyProcessingResult implements LispStruct {

	private final List<ListStruct> declarations;
	private final StringStruct docString;
	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final List<ListStruct> declarations, final StringStruct docString, final List<LispStruct> bodyForms) {
		this.declarations = declarations;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public List<ListStruct> getDeclarations() {
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
