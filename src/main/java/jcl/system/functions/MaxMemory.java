/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.math.BigInteger;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class MaxMemory extends FunctionStruct {

	public static final SymbolStruct<?> MAX_MEMORY = GlobalPackageStruct.EXTENSIONS.intern("MAX-MEMORY").getSymbol();

	private static final long serialVersionUID = -8202963218833948816L;

	private MaxMemory() {
		super("Returns the current max runtime memory usage.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MAX_MEMORY.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(MAX_MEMORY);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {
		return new OrdinaryLambdaList.Builder().build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final long maxMemory = Runtime.getRuntime().maxMemory();
		return new IntegerStruct(BigInteger.valueOf(maxMemory));
	}
}
