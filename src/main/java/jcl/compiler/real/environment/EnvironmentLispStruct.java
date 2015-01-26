package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.compiler.real.sa.element.declaration.DeclareElement;
import jcl.lists.ListStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentLispStruct implements LispStruct {

	private static final long serialVersionUID = -2569718491014694273L;

	protected final Environment environment;
	protected final DeclareElement declareElement;
	protected final ListStruct bodyForms;

	public EnvironmentLispStruct(final Environment environment, final DeclareElement declareElement, final ListStruct bodyForms) {
		this.environment = environment;
		this.declareElement = declareElement;
		this.bodyForms = bodyForms;
	}

	public Environment getEnvironment() {
		return environment;
	}

	public DeclareElement getDeclareElement() {
		return declareElement;
	}

	public ListStruct getBodyForms() {
		return bodyForms;
	}

	@Override
	public String printStruct() {
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE)
				+ "\nBody Printed: " + bodyForms.printStruct();
	}
}
