package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class KeyBinding extends ParameterBinding {

	private static final long serialVersionUID = -8247621420473541525L;

	private final KeywordSymbolStruct keyName;

	private final SuppliedPBinding suppliedPBinding;

	public KeyBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm,
	                  final KeywordSymbolStruct keyName, final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, allocation, Scope.LEXICAL, T.INSTANCE, initForm);
		this.keyName = keyName;
		this.suppliedPBinding = suppliedPBinding;
	}

	public KeywordSymbolStruct getKeyName() {
		return keyName;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
