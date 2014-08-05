package jcl.reader.function;

import jcl.LispStruct;
import jcl.reader.impl.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.RationalStruct;
import jcl.structs.symbols.variables.Variable;
import org.apache.commons.lang3.Range;

import java.math.BigInteger;

public class RationalReader {

	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(2), BigInteger.valueOf(36));

	private final Reader reader;

	public RationalReader(final Reader reader) {
		this.reader = reader;
	}

	public RationalStruct readRationalToken(final BigInteger radix) {
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			final ExtendedTokenReader macroFunctionReader = new ExtendedTokenReader(reader);
			macroFunctionReader.readExtendedToken(false);
			return null;
		}

		if (radix == null) {
			throw new ReaderErrorException("Radix missing in #R.");
		}

		if (!RADIX_RANGE.contains(radix)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		}

		final IntegerStruct previousReadBase = Variable.READ_BASE.getValue();

		// alter the read-base
		Variable.READ_BASE.setValue(new IntegerStruct(radix));

		// read rational
		final LispStruct lispToken = reader.read();

		// reset the read-base
		Variable.READ_BASE.setValue(previousReadBase);

		if (lispToken instanceof RationalStruct) {
			return (RationalStruct) lispToken;
		}

		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + lispToken + '.');
	}
}
