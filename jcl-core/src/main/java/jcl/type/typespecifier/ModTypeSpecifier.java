/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type.typespecifier;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.type.TypeBaseClass;

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
	public boolean typeEquals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (obj instanceof IntegerStruct) {
			final IntegerStruct objectInteger = (IntegerStruct) obj;
			final BigInteger objectValue = objectInteger.toJavaBigInteger();

			if (objectValue.compareTo(BigInteger.ZERO) < 0) {
				return false;
			}

			final BigInteger integerValue = integerStruct.toJavaBigInteger();

			return objectValue.compareTo(integerValue) <= 0;
		}

		return false;
	}
}
