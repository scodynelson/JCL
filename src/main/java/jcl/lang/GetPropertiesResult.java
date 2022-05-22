/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Result wrapper object for {@link ListStruct} property retrieval operation.
 */
@Getter
@AllArgsConstructor
public final class GetPropertiesResult {

	/**
	 * Entry indicator in the {@link ListStruct} property list. {@link NILStruct#INSTANCE} if no matchind indicator is
	 * found.
	 */
	private final LispStruct indicator;

	/**
	 * Entry value in the {@link ListStruct} property list. {@link NILStruct#INSTANCE} if no matching indicator is
	 * found.
	 */
	private final LispStruct value;

	/**
	 * Remainder of the {@link ListStruct} property list after an entry indicator is found. {@link NILStruct#INSTANCE}
	 * if no matching indicator is found.
	 */
	private final ListStruct tail;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #indicator}, {@link #value}, and {@link #tail} values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #indicator}, {@link #value}, and {@link #tail} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(indicator, value, tail);
	}
}
