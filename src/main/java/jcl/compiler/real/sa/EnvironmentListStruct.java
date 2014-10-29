package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.structs.arrays.StringStruct;
import jcl.structs.lists.ListStruct;

import java.util.List;

public class EnvironmentListStruct extends ListStruct {

	private final Environment environment;
	private final List<LispStruct> declarations;
	private final StringStruct docString;
	private final ListStruct bodyForms;

	public EnvironmentListStruct(final Environment environment, final List<LispStruct> declarations,
	                             final StringStruct docString, final ListStruct bodyForms) {
		super(null, null);
		this.environment = environment;
		this.declarations = declarations;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public Environment getEnvironment() {
		return environment;
	}

	public List<LispStruct> getDeclarations() {
		return declarations;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public ListStruct getBodyForms() {
		return bodyForms;
	}

	@Override
	public int size() {
		return bodyForms.size();
	}

	@Override
	public LispStruct getFirst() {
		return bodyForms.getFirst();
	}

	@Override
	public ListStruct getRest() {
		return bodyForms.getRest();
	}

	@Override
	public LispStruct getElement(final int index) {
		return bodyForms.getElement(index);
	}

	@Override
	public void setElement(final int index, final LispStruct newValue) {
		bodyForms.setElement(index, newValue);
	}

	@Override
	public List<LispStruct> getAsJavaList() {
		return bodyForms.getAsJavaList();
	}

	@Override
	public boolean isDotted() {
		return bodyForms.isDotted();
	}

	@Override
	public boolean isCircular() {
		return bodyForms.isCircular();
	}

	@Override
	public String printStruct() {
		return toString();
	}
}
