package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.symbols.KeywordSymbolStruct;
import jcl.types.T;

public class KeyBinding extends FunctionBinding {

	private final KeywordSymbolStruct keyName;

	public KeyBinding(final int allocationPosition, final LispStruct initForm, final KeywordSymbolStruct keyName) {
		super(LambdaListParser.KEY, allocationPosition, Scope.LEXICAL, T.INSTANCE, initForm);
		this.keyName = keyName;
	}

	public KeywordSymbolStruct getKeyName() {
		return keyName;
	}
}
