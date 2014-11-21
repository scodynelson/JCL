package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.numbers.ComplexStruct;
import jcl.structs.numbers.FloatStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.RatioStruct;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;
import java.util.List;

/**
 * Implements the '#c' Lisp reader macro.
 */
public final class SharpCReaderMacroFunction extends ReaderMacroFunction {

	public static final SharpCReaderMacroFunction INSTANCE = new SharpCReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpCReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_C) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct lispToken = reader.read();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (!(lispToken instanceof ListStruct)) {
			throw new ReaderErrorException("Illegal complex number format: #C" + lispToken);
		}

		final ListStruct listToken = (ListStruct) lispToken;
		final List<LispStruct> lispTokens = listToken.getAsJavaList();
		if (lispTokens.size() != 2) {
			throw new ReaderErrorException("Illegal complex number format: #C" + lispToken);
		}

		final LispStruct real = lispTokens.get(0);
		final LispStruct imaginary = lispTokens.get(1);

		// TODO: we REALLY need to do this better!!
		if ((real instanceof IntegerStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct((IntegerStruct) real, (IntegerStruct) imaginary);
		} else if ((real instanceof IntegerStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct((IntegerStruct) real, (FloatStruct) imaginary);
		} else if ((real instanceof IntegerStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct((IntegerStruct) real, (RatioStruct) imaginary);
		} else if ((real instanceof FloatStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct((FloatStruct) real, (IntegerStruct) imaginary);
		} else if ((real instanceof FloatStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct((FloatStruct) real, (FloatStruct) imaginary);
		} else if ((real instanceof FloatStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct((FloatStruct) real, (RatioStruct) imaginary);
		} else if ((real instanceof RatioStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct((RatioStruct) real, (IntegerStruct) imaginary);
		} else if ((real instanceof RatioStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct((RatioStruct) real, (FloatStruct) imaginary);
		} else if ((real instanceof RatioStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct((RatioStruct) real, (RatioStruct) imaginary);
		} else {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c.");
		}
	}
}
