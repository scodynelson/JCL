package jcl.structs.symbols.special;

import jcl.structs.numbers.IntegerStruct;
import jcl.structs.packages.PackageStruct;
import jcl.structs.symbols.Variable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

public class NonNegNilVariable extends Variable<IntegerStruct> {

	private static final Logger LOGGER = LoggerFactory.getLogger(NonNegNilVariable.class);

	public NonNegNilVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, null);
	}

	@Override
	public void setValue(final IntegerStruct value) {

		final BigInteger bigIntegerValue = value.getBigInteger();
		if (bigIntegerValue.compareTo(BigInteger.ZERO) >= 0) {
			this.value = value;
		} else {
			LOGGER.warn("Error: {} had illegal value {}.  Reset to NIL", name, value);

			this.value = null;
		}
	}
}
