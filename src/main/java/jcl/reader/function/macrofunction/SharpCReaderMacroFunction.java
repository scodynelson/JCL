package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.numbers.ComplexStruct;
import jcl.structs.numbers.FloatStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.RatioStruct;
import jcl.reader.ReadSuppressVariable;
import jcl.reader.impl.Reader;
import jcl.syntax.CharacterConstants;

import java.util.List;

/**
 * Implements the '#c' Lisp reader macro.
 */
public class SharpCReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_C) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct lispToken = reader.read();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
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
			throw new ReaderErrorException("Only reals are valid tokens for #c.");
		}
	}
}
