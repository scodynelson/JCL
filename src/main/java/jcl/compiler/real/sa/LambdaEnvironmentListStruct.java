package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.structs.arrays.StringStruct;
import jcl.structs.lists.ListStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class LambdaEnvironmentListStruct extends EnvironmentListStruct {

	protected final OrdinaryLambdaListBindings lambdaListBindings;
	protected final StringStruct docString;

	public LambdaEnvironmentListStruct(final Environment environment, final List<LispStruct> declarations, final ListStruct bodyForms,
									   final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString) {
		super(environment, declarations, bodyForms);
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
