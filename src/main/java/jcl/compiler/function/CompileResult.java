/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.ValuesStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public final class CompileResult {

	private final FunctionStruct function;
	private final BooleanStruct warningsP;
	private final BooleanStruct failureP;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #function}, {@link #warningsP}, and {@link #failureP}
	 * values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #function}, {@link #warningsP}, and {@link #failureP} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(function, warningsP, failureP);
	}
}
