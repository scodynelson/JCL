/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.Objects;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * Result wrapper object for {@link ArrayStruct} array displacement information.
 */
@Getter
@NoArgsConstructor(access = AccessLevel.PRIVATE)
@AllArgsConstructor
public final class ArrayDisplacement {

	/**
	 * Default return value for when there are no values for {@link #displacedTo} and {@link #displacedIndexOffset}.
	 */
	public static final ArrayDisplacement DEFAULT = new ArrayDisplacement();

	/**
	 * The {@link ArrayStruct} structure that this array is displaced to. If {@code null}, this structure is not
	 * displaced to anything.
	 */
	private ArrayStruct displacedTo;

	/**
	 * The index offset into the {@link #displacedTo} structure when looking for or updating elements.
	 */
	private IntegerStruct displacedIndexOffset;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #displacedTo} and {@link #displacedIndexOffset} values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #displacedTo} and {@link #displacedIndexOffset} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(
				Objects.requireNonNullElse(displacedTo, NILStruct.INSTANCE),
				Objects.requireNonNullElse(displacedIndexOffset, IntegerStruct.ZERO)
		);
	}
}
