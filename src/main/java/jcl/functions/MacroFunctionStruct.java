/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.MacroLambdaList;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;

public abstract class MacroFunctionStruct extends FunctionStruct {

	protected final MacroLambdaList macroLambdaList;

	protected final MacroFunctionExpander<?> macroFunctionExpander;

	/**
	 * Protected constructor.
	 */
	protected MacroFunctionStruct() {
		this(null, null, null, null);
	}

	protected MacroFunctionStruct(final String documentation, final Closure closure) {
		this(documentation, closure, null, null);
	}

	protected MacroFunctionStruct(final MacroLambdaList macroLambdaList, final MacroFunctionExpander<?> macroFunctionExpander) {
		this(null, null, macroLambdaList, macroFunctionExpander);
	}

	protected MacroFunctionStruct(final String documentation, final MacroLambdaList lambdaListBindings,
	                              final MacroFunctionExpander<?> macroFunctionExpander) {
		this(documentation, null, lambdaListBindings, macroFunctionExpander);
	}

	protected MacroFunctionStruct(final Closure closure, final MacroLambdaList lambdaListBindings,
	                              final MacroFunctionExpander<?> macroFunctionExpander) {
		this(null, closure, lambdaListBindings, macroFunctionExpander);
	}

	protected MacroFunctionStruct(final String documentation, final Closure closure, final MacroLambdaList macroLambdaList,
	                              final MacroFunctionExpander<?> macroFunctionExpander) {
//		super(documentation, closure);
		this.macroLambdaList = macroLambdaList;
		this.macroFunctionExpander = macroFunctionExpander;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final ListStruct listStruct = (ListStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return macroFunctionExpander.expand(listStruct, environment);
	}
}
