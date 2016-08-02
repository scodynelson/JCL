package jcl.lang.statics;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.VariableStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.number.IntegerStructImpl;
import org.apache.commons.lang3.Range;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RadixVariable extends VariableStruct<IntegerStructImpl> {

	private static final int lowerBound = 2;

	private static final int upperBound = 32;

	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(lowerBound), BigInteger.valueOf(upperBound));

	private static final Logger LOGGER = LoggerFactory.getLogger(RadixVariable.class);

	public RadixVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, IntegerStructImpl.TEN);
	}

	@Override
	public void setValue(final LispStruct value) {
		if (!(value instanceof IntegerStructImpl)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be Integer value.");
		}
		final IntegerStructImpl variableValue = (IntegerStructImpl) value;

		// TODO: optimize??
		final BigInteger bigIntegerValue = variableValue.bigIntegerValue();
		if (RADIX_RANGE.contains(bigIntegerValue)) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to 10", name, variableValue);

			super.setValue(IntegerStructImpl.TEN);
		}
	}
}
