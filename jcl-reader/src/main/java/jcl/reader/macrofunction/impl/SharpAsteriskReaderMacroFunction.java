package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReadExtendedToken;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.arrays.BitVectorStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.types.Variable;
import org.apache.commons.lang3.StringUtils;

/**
 * Implements the '#*' Lisp reader macro.
 */
public class SharpAsteriskReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert codePoint == CharacterConstants.ASTERISK;

		final ReadExtendedToken readExtendedToken = reader.readExtendedToken();

		if (Variable.ReadSuppress) {
			return null;
		}

		if (readExtendedToken.hasEscapes()) {
			throw new ReaderErrorException("Escape character appeared after #*");
		}

		final String bitString = readExtendedToken.getToken();

		if (numArg == null) {
			try {
				return new BitVectorStruct(bitString);
			} catch (final TypeErrorException | SimpleErrorException e) {
				throw new ReaderErrorException("Error occurred creating bit-vector.", e);
			}
		}

		final int bitStringLength = bitString.length();

		if (StringUtils.isEmpty(bitString)) {
			throw new ReaderErrorException("At least one bit must be supplied for non-zero #* bit-vectors.");
		} else if (bitStringLength > numArg) {
			throw new ReaderErrorException("Bit vector is longer than specified length: #" + numArg + '*' + bitString);
		} else {
			final char lastChar = bitString.charAt(bitStringLength - 1);

			final StringBuilder bitStringBuilder = new StringBuilder(bitString);

			final int fillAmount = numArg - bitStringLength;
			for (int i = 0; i < fillAmount; i++) {
				bitStringBuilder.append(lastChar);
			}

			final String newBitString = bitStringBuilder.toString();
			try {
				return new BitVectorStruct(newBitString);
			} catch (final TypeErrorException | SimpleErrorException e) {
				throw new ReaderErrorException("Error occurred creating bit-vector.", e);
			}
		}
	}
}
