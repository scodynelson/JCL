/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaListBindings;
import jcl.functions.expanders.MacroFunctionExpander;

public abstract class MacroFunctionStruct extends FunctionStruct {

	private static final long serialVersionUID = -5950370781257065737L;

	protected final MacroLambdaListBindings macroLambdaListBindings;

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

	protected MacroFunctionStruct(final MacroLambdaListBindings macroLambdaListBindings, final MacroFunctionExpander<?> macroFunctionExpander) {
		this(null, null, macroLambdaListBindings, macroFunctionExpander);
	}

	protected MacroFunctionStruct(final String documentation, final MacroLambdaListBindings lambdaListBindings,
	                              final MacroFunctionExpander<?> macroFunctionExpander) {
		this(documentation, null, lambdaListBindings, macroFunctionExpander);
	}

	protected MacroFunctionStruct(final Closure closure, final MacroLambdaListBindings lambdaListBindings,
	                              final MacroFunctionExpander<?> macroFunctionExpander) {
		this(null, closure, lambdaListBindings, macroFunctionExpander);
	}

	protected MacroFunctionStruct(final String documentation, final Closure closure, final MacroLambdaListBindings macroLambdaListBindings,
	                              final MacroFunctionExpander<?> macroFunctionExpander) {
		super(documentation, closure);
		this.macroLambdaListBindings = macroLambdaListBindings;
		this.macroFunctionExpander = macroFunctionExpander;
	}
}
