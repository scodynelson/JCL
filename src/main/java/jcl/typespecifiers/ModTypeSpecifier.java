package jcl.typespecifiers;

import jcl.structs.numbers.IntegerStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.types.TypeBaseClass;

import java.math.BigInteger;

/**
 * A {@link ModTypeSpecifier} denotes the set of non-negative integers less than n. This is equivalent to (integer 0 (n))
 * or to (integer 0 m), where m=n-1. The argument is required, and cannot be *. The symbol mod is not valid as a type specifier.
 */
public class ModTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	private final IntegerStruct integerStruct;

	/**
	 * Public constructor.
	 *
	 * @param integerStruct the integer structure
	 */
	public ModTypeSpecifier(final IntegerStruct integerStruct) {
		this("T", integerStruct); // TODO: Should this be 'T'???
	}

	/**
	 * Protected constructor.
	 *
	 * @param name          the name of the symbol type
	 * @param integerStruct the integer structure
	 */
	protected ModTypeSpecifier(final String name, final IntegerStruct integerStruct) {
		super(name, GlobalPackageStruct.COMMON_LISP);
		this.integerStruct = integerStruct;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (obj instanceof IntegerStruct) {
			final IntegerStruct objectInteger = (IntegerStruct) obj;
			final BigInteger objectValue = objectInteger.getBigInteger();

			if (objectValue.compareTo(BigInteger.ZERO) == -1) {
				return false;
			}

			final BigInteger integerValue = integerStruct.getBigInteger();

			return objectValue.compareTo(integerValue) <= 0;
		}

		return false;
	}

	@Override
	public int hashCode() {
		return integerStruct.hashCode();
	}

	@Override
	public String toString() {
		return "ModTypeSpecifier{"
				+ "integerStruct=" + integerStruct
				+ '}';
	}
}
