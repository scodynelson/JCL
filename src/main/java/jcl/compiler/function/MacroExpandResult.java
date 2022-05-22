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
public final class MacroExpandResult {

	private final LispStruct expandedForm;
	private final BooleanStruct wasExpanded;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #expandedForm} and {@link #expandedForm} values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #expandedForm} and {@link #wasExpanded} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(expandedForm, wasExpanded);
	}
}
