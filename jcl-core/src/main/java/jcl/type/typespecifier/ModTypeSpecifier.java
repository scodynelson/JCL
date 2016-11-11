/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type.typespecifier;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.type.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * A {@link ModTypeSpecifier} denotes the set of non-negative integers less than n. This is equivalent to (integer 0
 * (n)) or to (integer 0 m), where m=n-1. The argument is required, and cannot be *. The symbol mod is not valid as a
 * type specifier.
 */
public class ModTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	/**
	 * The {@link IntegerStruct} to check against the 'MOD' type specifier.
	 */
	private final IntegerStruct integerStruct;

	/**
	 * Public constructor.
	 *
	 * @param integerStruct
	 * 		the integer structure
	 */
	public ModTypeSpecifier(final IntegerStruct integerStruct) {
		this("T", integerStruct); // TODO: Should this be 'T'???
	}

	/**
	 * Protected constructor.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param integerStruct
	 * 		the integer structure
	 */
	protected ModTypeSpecifier(final String name, final IntegerStruct integerStruct) {
		super(name);
		this.integerStruct = integerStruct;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(integerStruct)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (obj instanceof IntegerStruct) {
			final IntegerStruct objectInteger = (IntegerStruct) obj;
			final BigInteger objectValue = objectInteger.bigIntegerValue();

			if (objectValue.compareTo(BigInteger.ZERO) == -1) {
				return false;
			}

			final BigInteger integerValue = integerStruct.bigIntegerValue();

			return objectValue.compareTo(integerValue) <= 0;
		}

		return false;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(integerStruct)
		                                                                .toString();
	}
}
