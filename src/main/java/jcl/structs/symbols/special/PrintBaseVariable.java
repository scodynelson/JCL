package jcl.structs.symbols.special;

import jcl.structs.numbers.IntegerStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.Variable;
import org.apache.commons.lang3.Range;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

public class PrintBaseVariable extends Variable<IntegerStruct> {

	public static final PrintBaseVariable INSTANCE = new PrintBaseVariable();

	private static final IntegerStruct TEN = new IntegerStruct(BigInteger.TEN);

	private static final int lowerBound = 2;
	private static final int upperBound = 32;
	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(lowerBound), BigInteger.valueOf(upperBound));

	private static final Logger LOGGER = LoggerFactory.getLogger(PrintBaseVariable.class);

	private PrintBaseVariable() {
		super("*PRINT-BASE*", GlobalPackageStruct.COMMON_LISP, TEN);
	}

	@Override
	public void setValue(final IntegerStruct value) {

		final BigInteger bigIntegerValue = value.getBigInteger();
		if (RADIX_RANGE.contains(bigIntegerValue)) {
			this.value = value;
		} else {
			LOGGER.warn("Error: *PRINT-BASE* had illegal value {}.  Reset to 10", value);

			this.value = TEN;
		}
	}
}
