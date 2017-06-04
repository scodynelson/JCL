package jcl.lang.statics;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VariableStructImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class NonNegNilVariable extends VariableStructImpl<IntegerStruct> {

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

		// TODO: optimize??
		final BigInteger bigIntegerValue = variableValue.toJavaBigInteger();
		if (bigIntegerValue.compareTo(BigInteger.ZERO) >= 0) {
			super.setValue(variableValue);
		} else {
			LOGGER.warn("Error: {} had illegal value {}. Reset to NIL", name, variableValue);

			// TODO: check this set here...
			super.setValue(null);
		}
	}
}
