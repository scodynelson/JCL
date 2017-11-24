/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import jcl.lang.FunctionStruct;

public class CompileResult {

	private final FunctionStruct function;

	private final boolean compiledWithWarnings;

	private final boolean failedToCompile;

	public CompileResult(final FunctionStruct function, final boolean compiledWithWarnings, final boolean failedToCompile) {
		this.function = function;
		this.compiledWithWarnings = compiledWithWarnings;
		this.failedToCompile = failedToCompile;
	}

	public FunctionStruct getFunction() {
		return function;
	}

	public boolean isCompiledWithWarnings() {
		return compiledWithWarnings;
	}

	public boolean isFailedToCompile() {
		return failedToCompile;
	}
}
