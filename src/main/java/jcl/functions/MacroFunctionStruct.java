/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaListBindings;
import jcl.functions.expanders.MacroExpander;

public abstract class MacroFunctionStruct extends FunctionStruct {

	private static final long serialVersionUID = -5950370781257065737L;

	protected final MacroLambdaListBindings macroLambdaListBindings;

	protected final MacroExpander<?, ?> macroExpander;

	/**
	 * Protected constructor.
	 */
	protected MacroFunctionStruct() {
		this(null, null, null, null);
	}

	protected MacroFunctionStruct(final String documentation, final Closure closure) {
		this(documentation, closure, null, null);
	}

	protected MacroFunctionStruct(final MacroLambdaListBindings macroLambdaListBindings, final MacroExpander<?, ?> macroExpander) {
		this.macroLambdaListBindings = macroLambdaListBindings;
		this.macroExpander = macroExpander;
	}

	protected MacroFunctionStruct(final String documentation, final MacroLambdaListBindings lambdaListBindings,
	                              final MacroExpander<?, ?> macroExpander) {
		this(documentation, null, lambdaListBindings, macroExpander);
	}

	protected MacroFunctionStruct(final Closure closure, final MacroLambdaListBindings lambdaListBindings,
	                              final MacroExpander<?, ?> macroExpander) {
		this(null, closure, lambdaListBindings, macroExpander);
	}

	protected MacroFunctionStruct(final String documentation, final Closure closure, final MacroLambdaListBindings macroLambdaListBindings,
	                              final MacroExpander<?, ?> macroExpander) {
		super(documentation, closure);
		this.macroLambdaListBindings = macroLambdaListBindings;
		this.macroExpander = macroExpander;
	}
}
