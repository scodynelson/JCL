package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.types.T;

public class RequiredBinding extends FunctionBinding {

	public RequiredBinding(final int allocationPosition, final LispStruct initForm) {
		super(LambdaListParser.REQUIRED, allocationPosition, Scope.LEXICAL, T.INSTANCE, initForm);
	}
}
