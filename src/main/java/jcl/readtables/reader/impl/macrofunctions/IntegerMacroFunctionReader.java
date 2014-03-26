package jcl.readtables.reader.impl.macrofunctions;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.numbers.IntegerStruct;
import jcl.numbers.ReadBaseVariable;
import jcl.readtables.reader.LispReader;
import jcl.variables.ReadSuppressVariable;

public class IntegerMacroFunctionReader {

	private final LispReader reader;

	public IntegerMacroFunctionReader(final LispReader reader) {
		this.reader = reader;
	}

	public IntegerStruct readIntegerToken(final Integer radix) {
		if (ReadSuppressVariable.INSTANCE.getValue()) {
			final ExtendedTokenMacroFunctionReader macroFunctionReader = new ExtendedTokenMacroFunctionReader(reader);
			macroFunctionReader.readExtendedToken();
			return null;
		} else if (radix == null) {
			throw new ReaderErrorException("Radix missing in #R.");
		} else if ((radix < 2) && (radix > 36)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		} else {
			final int previousReadBase = ReadBaseVariable.INSTANCE.getValue();

			// alter the readbase
			ReadBaseVariable.INSTANCE.setValue(radix);

			// read integer
			final LispStruct lispToken = reader.read();
			if (lispToken instanceof IntegerStruct) {

				final IntegerStruct integerToken = (IntegerStruct) lispToken;

				// reset the readbase
				ReadBaseVariable.INSTANCE.setValue(previousReadBase);

				return integerToken;
			} else {
				// reset the readbase
				ReadBaseVariable.INSTANCE.setValue(previousReadBase);

				throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + lispToken + '.');
			}
		}
	}
}
