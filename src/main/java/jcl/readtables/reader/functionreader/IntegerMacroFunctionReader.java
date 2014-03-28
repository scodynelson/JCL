package jcl.readtables.reader.functionreader;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.numbers.IntegerStruct;
import jcl.numbers.ReadBaseVariable;
import jcl.readtables.reader.Reader;
import jcl.variables.ReadSuppressVariable;
import org.apache.commons.lang3.Range;

public class IntegerMacroFunctionReader {

	private static final Range<Integer> RADIX_RANGE = Range.between(2, 36);

	private final Reader reader;

	public IntegerMacroFunctionReader(final Reader reader) {
		this.reader = reader;
	}

	public IntegerStruct readIntegerToken(final Integer radix) {
		if (ReadSuppressVariable.INSTANCE.getValue()) {
			final ExtendedTokenMacroFunctionReader macroFunctionReader = new ExtendedTokenMacroFunctionReader(reader);
			macroFunctionReader.readExtendedToken(false);
			return null;
		}

		if (radix == null) {
			throw new ReaderErrorException("Radix missing in #R.");
		}

		if (!RADIX_RANGE.contains(radix)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		}

		final int previousReadBase = ReadBaseVariable.INSTANCE.getValue();

		// alter the readbase
		ReadBaseVariable.INSTANCE.setValue(radix);

		// read integer
		final LispStruct lispToken = reader.read();

		// reset the readbase
		ReadBaseVariable.INSTANCE.setValue(previousReadBase);

		if (lispToken instanceof IntegerStruct) {
			return (IntegerStruct) lispToken;
		}

		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + lispToken + '.');

	}
}
