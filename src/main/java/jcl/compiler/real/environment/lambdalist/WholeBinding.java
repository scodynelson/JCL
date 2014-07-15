package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.types.List;

public class WholeBinding extends FunctionBinding {

	public WholeBinding(final int allocationPosition, final LispStruct initForm) {
		super(LambdaListParser.WHOLE, allocationPosition, Scope.LEXICAL, List.INSTANCE, initForm);
	}
}
