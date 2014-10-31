package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.structs.lists.ListStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class EnvironmentListStruct extends ListStruct {

	protected final Environment environment;
	protected final List<LispStruct> declarations;
	protected final ListStruct bodyForms;

	public EnvironmentListStruct(final Environment environment, final List<LispStruct> declarations, final ListStruct bodyForms) {
		super(null, null);
		this.environment = environment;
		this.declarations = declarations;
		this.bodyForms = bodyForms;
	}

	public Environment getEnvironment() {
		return environment;
	}

	public List<LispStruct> getDeclarations() {
		return declarations;
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
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE)
				+ "\nBody Printed: " + bodyForms.printStruct();
	}
}
