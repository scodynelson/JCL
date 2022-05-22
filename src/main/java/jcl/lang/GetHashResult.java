/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Result wrapper object for {@link HashTableStruct} value retrieval operation.
 */
@Getter
@AllArgsConstructor
public final class GetHashResult {

	/**
	 * The value retrieved from the hash-table.
	 */
	private final LispStruct value;

	/**
	 * Whether or not the value was present in the hash-table.
	 */
	private final BooleanStruct presentP;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #value} and {@link #presentP} values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #value} and {@link #presentP} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(value, presentP);
	}
}
