/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public final class CompileFileResult {

	private final LispStruct outputTruename;
	private final BooleanStruct warningsP;
	private final BooleanStruct failureP;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #outputTruename}, {@link #warningsP}, and {@link #failureP}
	 * values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #outputTruename}, {@link #warningsP}, and {@link #failureP} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(outputTruename, warningsP, failureP);
	}
}
