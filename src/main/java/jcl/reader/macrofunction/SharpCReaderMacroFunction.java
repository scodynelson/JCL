package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.MacroFunctionReader;
import jcl.reader.ReaderMacroFunction;
import jcl.syntax.CharacterConstants;
import jcl.numbers.ComplexStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.lists.ListStruct;
import jcl.numbers.RatioStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.variables.ReadSuppressVariable;

import java.util.List;

/**
 * Implements the '#c' Lisp reader macro.
 */
public class SharpCReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
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
			return new ComplexStruct(((IntegerStruct) real).getBigInteger(), ((IntegerStruct) imaginary).getBigInteger());
		} else if ((real instanceof IntegerStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct(((IntegerStruct) real).getBigInteger(), ((FloatStruct) imaginary).getBigDecimal());
		} else if ((real instanceof IntegerStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct(((IntegerStruct) real).getBigInteger(), ((RatioStruct) imaginary).getBigFraction());
		} else if ((real instanceof FloatStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct(((FloatStruct) real).getBigDecimal(), ((IntegerStruct) imaginary).getBigInteger());
		} else if ((real instanceof FloatStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct(((FloatStruct) real).getBigDecimal(), ((FloatStruct) imaginary).getBigDecimal());
		} else if ((real instanceof FloatStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct(((FloatStruct) real).getBigDecimal(), ((RatioStruct) imaginary).getBigFraction());
		} else if ((real instanceof RatioStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct(((RatioStruct) real).getBigFraction(), ((IntegerStruct) imaginary).getBigInteger());
		} else if ((real instanceof RatioStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct(((RatioStruct) real).getBigFraction(), ((FloatStruct) imaginary).getBigDecimal());
		} else if ((real instanceof RatioStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct(((RatioStruct) real).getBigFraction(), ((RatioStruct) imaginary).getBigFraction());
		} else {
			throw new ReaderErrorException("Only reals are valid tokens for #c.");
		}
	}
}
