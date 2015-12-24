package jcl.printer;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.numbers.IntegerStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.VariableStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class NonNegNilVariable extends VariableStruct<IntegerStruct> {

	private static final long serialVersionUID = -7673142742585929799L;

	private static final Logger LOGGER = LoggerFactory.getLogger(NonNegNilVariable.class);

	NonNegNilVariable(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage, null);
	}

	@Override
	public void setValue(final LispStruct value) {
		if (!(value instanceof IntegerStruct)) {
			// TODO: Fix me
			throw new TypeErrorException("Must be Integer value.");
		}
		final IntegerStruct variableValue = (IntegerStruct) value;

		final BigInteger bigIntegerValue = variableValue.getBigInteger();
		if (bigIntegerValue.compareTo(BigInteger.ZERO) >= 0) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to NIL", name, variableValue);

			// TODO: check this set here...
			super.setValue(null);
		}
	}
}
