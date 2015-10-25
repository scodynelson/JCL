/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.math.BigInteger;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class FreeMemory extends FunctionStruct {

	public static final SymbolStruct<?> FREE_MEMORY = GlobalPackageStruct.EXTENSIONS.intern("FREE-MEMORY").getSymbol();

	private static final long serialVersionUID = -3953991334787306041L;

	private FreeMemory() {
		super("Returns the current free runtime memory usage.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		FREE_MEMORY.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(FREE_MEMORY);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {
		return new OrdinaryLambdaList.Builder().build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final long freeMemory = Runtime.getRuntime().freeMemory();
		return new IntegerStruct(BigInteger.valueOf(freeMemory));
	}
}
