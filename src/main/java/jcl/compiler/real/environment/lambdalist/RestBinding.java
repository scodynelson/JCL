package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.types.List;

public class RestBinding extends FunctionBinding {

	public RestBinding(final int allocationPosition, final LispStruct initForm) {
		super(LambdaListParser.REST, allocationPosition, Scope.LEXICAL, List.INSTANCE, initForm);
	}
}
