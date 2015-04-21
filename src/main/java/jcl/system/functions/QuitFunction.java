/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class QuitFunction extends FunctionStruct {

	public static final SymbolStruct<?> QUIT = GlobalPackageStruct.EXTENSIONS.intern("QUIT").getSymbol();

	private static final long serialVersionUID = -7684846282276245122L;

	private QuitFunction() {
		super("Quits the JCL Application.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		QUIT.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(QUIT);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {
		return new OrdinaryLambdaListBindings.Builder().build();
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		System.exit(0);
		return null;
	}
}
