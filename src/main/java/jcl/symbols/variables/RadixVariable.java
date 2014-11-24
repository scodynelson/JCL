package jcl.symbols.variables;

import jcl.numbers.IntegerStruct;
import jcl.packages.PackageStruct;
import org.apache.commons.lang3.Range;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

class RadixVariable extends Variable<IntegerStruct> {

	private static final IntegerStruct TEN = new IntegerStruct(BigInteger.TEN);

	private static final int lowerBound = 2;
	private static final int upperBound = 32;
	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(lowerBound), BigInteger.valueOf(upperBound));

	private static final Logger LOGGER = LoggerFactory.getLogger(RadixVariable.class);

	RadixVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, TEN);
	}

	@Override
	public void setValue(final IntegerStruct value) {

		final BigInteger bigIntegerValue = value.getBigInteger();
		if (RADIX_RANGE.contains(bigIntegerValue)) {
			this.value = value;
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to 10", name, value);

			this.value = TEN;
		}
	}
}
