/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class UnbindSymbolFunctionFunction extends AbstractSystemFunctionStruct {

	public static final SymbolStruct<?> UNBIND_SYMBOL_FUNCTION = GlobalPackageStruct.SYSTEM.intern("UNBIND-SYMBOL-FUNCTION").getSymbol();

	private static final long serialVersionUID = -5643412206135382825L;

	public UnbindSymbolFunctionFunction() {
		super("Unbinds the function value of the provided symbol from its current value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.SYSTEM, "SYMBOL").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		return symbol.unbindFunction();
	}

	@Override
	protected String functionName() {
		return "UNBIND-SYMBOL-FUNCTION";
	}
}
