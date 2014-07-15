package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.types.T;

public class EnvironmentBinding extends FunctionBinding {

	public EnvironmentBinding(final int allocationPosition, final LispStruct initForm) {
		super(LambdaListParser.ENVIRONMENT, allocationPosition, Scope.LEXICAL, T.INSTANCE, initForm);
	}
}
