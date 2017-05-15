/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;

public class CompileResult {

	private final FunctionStruct function;

	private final BooleanStruct compiledWithWarnings;

	private final BooleanStruct failedToCompile;

	public CompileResult(final FunctionStruct function, final BooleanStruct compiledWithWarnings, final BooleanStruct failedToCompile) {
		this.function = function;
		this.compiledWithWarnings = compiledWithWarnings;
		this.failedToCompile = failedToCompile;
	}

	public FunctionStruct getFunction() {
		return function;
	}

	public BooleanStruct isCompiledWithWarnings() {
		return compiledWithWarnings;
	}

	public BooleanStruct isFailedToCompile() {
		return failedToCompile;
	}
}
