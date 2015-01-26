package jcl.printer;

import jcl.numbers.IntegerStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.Variable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

class NonNegNilVariable extends Variable<IntegerStruct> {

	private static final long serialVersionUID = -7673142742585929799L;

	private static final Logger LOGGER = LoggerFactory.getLogger(NonNegNilVariable.class);

	NonNegNilVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, null);
	}

	@Override
	public void setValue(final IntegerStruct value) {

		final BigInteger bigIntegerValue = value.getBigInteger();
		if (bigIntegerValue.compareTo(BigInteger.ZERO) >= 0) {
			this.value = value;
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to NIL", name, value);

			this.value = null;
		}
	}
}
