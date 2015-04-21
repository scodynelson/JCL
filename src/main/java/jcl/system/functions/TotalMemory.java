/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.math.BigInteger;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class TotalMemory extends FunctionStruct {

	public static final SymbolStruct<?> TOTAL_MEMORY = GlobalPackageStruct.EXTENSIONS.intern("TOTAL-MEMORY").getSymbol();

	private static final long serialVersionUID = 8319997947442435520L;

	private TotalMemory() {
		super("Returns the current total runtime memory usage.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		TOTAL_MEMORY.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(TOTAL_MEMORY);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {
		return new OrdinaryLambdaListBindings.Builder().build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final long totalMemory = Runtime.getRuntime().totalMemory();
		return new IntegerStruct(BigInteger.valueOf(totalMemory));
	}
}
