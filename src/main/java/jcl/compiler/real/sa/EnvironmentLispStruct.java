package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.Environment;
import jcl.structs.lists.ListStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class EnvironmentLispStruct implements LispStruct {

	protected final Environment environment;
	protected final List<ListStruct> declarations;
	protected final ListStruct bodyForms;

	public EnvironmentLispStruct(final Environment environment, final List<ListStruct> declarations, final ListStruct bodyForms) {
		this.environment = environment;
		this.declarations = declarations;
		this.bodyForms = bodyForms;
	}

	public Environment getEnvironment() {
		return environment;
	}

	public List<ListStruct> getDeclarations() {
		return declarations;
	}

	public ListStruct getBodyForms() {
		return bodyForms;
	}

	@Override
	public LispType getType() {
		return null;
	}

	@Override
	public String printStruct() {
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE)
				+ "\nBody Printed: " + bodyForms.printStruct();
	}
}
