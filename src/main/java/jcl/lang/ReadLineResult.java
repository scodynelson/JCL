/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Result object from a 'read' operation performed via 'read-line' from an {@link InputStreamStruct}.
 */
@Getter
@AllArgsConstructor
public final class ReadLineResult {

	/**
	 * The {@link LispStruct} result of the read operation.
	 */
	private final LispStruct result;

	/**
	 * True if the result was terminated by EOF; false if the result was terminated by a newline.
	 */
	private final BooleanStruct missingNewlineP;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #result} and {@link #missingNewlineP} values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #result} and {@link #missingNewlineP} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(result, missingNewlineP);
	}
}
