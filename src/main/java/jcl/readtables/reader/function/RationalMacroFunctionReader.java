package jcl.readtables.reader.function;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.numbers.RationalStruct;
import jcl.numbers.ReadBaseVariable;
import jcl.readtables.reader.ReadSuppressVariable;
import jcl.readtables.reader.Reader;
import org.apache.commons.lang3.Range;

public class RationalMacroFunctionReader {

	private static final Range<Integer> RADIX_RANGE = Range.between(2, 36);

	private final Reader reader;

	public RationalMacroFunctionReader(final Reader reader) {
		this.reader = reader;
	}

	public RationalStruct readRationalToken(final Integer radix) {
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

		// alter the read-base
		ReadBaseVariable.INSTANCE.setValue(radix);

		// read rational
		final LispStruct lispToken = reader.read();

		// reset the read-base
		ReadBaseVariable.INSTANCE.setValue(previousReadBase);

		if (lispToken instanceof RationalStruct) {
			return (RationalStruct) lispToken;
		}

		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + lispToken + '.');

	}
}
