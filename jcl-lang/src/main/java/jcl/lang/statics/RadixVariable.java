package jcl.lang.statics;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.VariableStruct;
import jcl.lang.condition.exception.TypeErrorException;
import org.apache.commons.lang3.Range;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RadixVariable extends VariableStruct<IntegerStruct> {

	private static final int lowerBound = 2;

	private static final int upperBound = 32;

	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(lowerBound), BigInteger.valueOf(upperBound));

	private static final Logger LOGGER = LoggerFactory.getLogger(RadixVariable.class);

	public RadixVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, IntegerStruct.TEN);
	}

	@Override
	public void setValue(final LispStruct value) {
		if (!(value instanceof IntegerStruct)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be Integer value.");
		}
		final IntegerStruct variableValue = (IntegerStruct) value;

		// TODO: optimize??
		final BigInteger bigIntegerValue = variableValue.bigIntegerValue();
		if (RADIX_RANGE.contains(bigIntegerValue)) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to 10", name, variableValue);

			super.setValue(IntegerStruct.TEN);
		}
	}
}
