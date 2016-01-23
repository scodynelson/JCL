package jcl.printer;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.numbers.IntegerStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.VariableStruct;
import org.apache.commons.lang3.Range;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RadixVariable extends VariableStruct<IntegerStruct> {

	private static final IntegerStruct TEN = new IntegerStruct(BigInteger.TEN);

	private static final int lowerBound = 2;

	private static final int upperBound = 32;

	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(lowerBound), BigInteger.valueOf(upperBound));

	private static final Logger LOGGER = LoggerFactory.getLogger(RadixVariable.class);

	public RadixVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, TEN);
	}

	@Override
	public void setValue(final LispStruct value) {
		if (!(value instanceof IntegerStruct)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be Integer value.");
		}
		final IntegerStruct variableValue = (IntegerStruct) value;

		final BigInteger bigIntegerValue = variableValue.getBigInteger();
		if (RADIX_RANGE.contains(bigIntegerValue)) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to 10", name, variableValue);

			super.setValue(TEN);
		}
	}
}
