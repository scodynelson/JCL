package jcl.compiler.real.environment;

import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.sa.element.declaration.DeclareElement;
import jcl.lists.ListStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaEnvironmentLispStruct extends EnvironmentLispStruct {

	protected final OrdinaryLambdaListBindings lambdaListBindings;
	protected final StringStruct docString;

	public LambdaEnvironmentLispStruct(final Environment environment, final DeclareElement declareElement, final ListStruct bodyForms,
	                                   final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString) {
		super(environment, declareElement, bodyForms);
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	@Override
	public String printStruct() {
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE)
				+ "\nBody Printed: " + bodyForms.printStruct();
	}
}
