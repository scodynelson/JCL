package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;
import jcl.structs.arrays.StringStruct;
import jcl.structs.lists.ListStruct;

import java.util.List;

public class LambdaEnvironmentListStruct extends EnvironmentListStruct {

	private final OrdinaryLambdaListBindings lambdaListBindings;

	public LambdaEnvironmentListStruct(final Environment environment, final List<LispStruct> declarations,
	                                   final StringStruct docString, final ListStruct bodyForms,
	                                   final OrdinaryLambdaListBindings lambdaListBindings) {
		super(environment, declarations, docString, bodyForms);
		this.lambdaListBindings = lambdaListBindings;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}
}
