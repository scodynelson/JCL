package jcl.lang.statics;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VariableStructImpl;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.Range;

@Log4j2
public class RadixVariable extends VariableStructImpl<IntegerStruct> {

	private static final int lowerBound = 2;

	private static final int upperBound = 32;

	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(lowerBound), BigInteger.valueOf(upperBound));

	public RadixVariable(final String name) {
		super(name);
	}

	@Override
	public LispStruct setSymbolValue(final LispStruct value) {
		if (!(value instanceof IntegerStruct)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be Integer value.");
		}
		final IntegerStruct variableValue = (IntegerStruct) value;

		// TODO: optimize??
		final BigInteger bigIntegerValue = variableValue.toJavaBigInteger();
		if (RADIX_RANGE.contains(bigIntegerValue)) {
			return super.setSymbolValue(variableValue);
		} else {
			log.warn("Error: {} had illegal value {}. Reset to 10", name, variableValue);

			return super.setSymbolValue(IntegerStruct.TEN);
		}
	}
}
