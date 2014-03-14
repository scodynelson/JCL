package jcl.numbers;

import jcl.numbers.IntegerStruct;
import jcl.typespecifiers.CompoundTypeSpecifier;

import java.math.BigInteger;

/**
 * A {@code ModTypeSpecifier} denotes the set of non-negative integers less than n. This is equivalent to (integer 0 (n))
 * or to (integer 0 m), where m=n-1. The argument is required, and cannot be *. The symbol mod is not valid as a type specifier.
 */
public class ModTypeSpecifier implements CompoundTypeSpecifier {

	private final IntegerStruct integerStruct;

	/**
	 * Public constructor.
	 *
	 * @param integerStruct the integer structure
	 */
	public ModTypeSpecifier(final IntegerStruct integerStruct) {
		this.integerStruct = integerStruct;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof IntegerStruct)) {
			return false;
		}

		final IntegerStruct objectInteger = (IntegerStruct) obj;
		final BigInteger objectValue = objectInteger.getBigInteger();

		if (objectValue.compareTo(BigInteger.ZERO) == -1) {
			return false;
		}

		final BigInteger integerValue = integerStruct.getBigInteger();

		return objectValue.compareTo(integerValue) <= 0;
	}

	@Override
	public int hashCode() {
		return integerStruct.hashCode();
	}
}
