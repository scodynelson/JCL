/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class QuitFunction extends FunctionStruct {

	public static final SymbolStruct<?> QUIT = GlobalPackageStruct.EXTENSIONS.intern("QUIT").getSymbol();

	private static final long serialVersionUID = -7684846282276245122L;

	@Autowired
	private ApplicationContext context;

	private QuitFunction() {
		super("Quits the JCL Application.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		QUIT.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(QUIT);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {
		return new OrdinaryLambdaList.Builder().build();
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		System.exit(SpringApplication.exit(context));
		return null;
	}
}
