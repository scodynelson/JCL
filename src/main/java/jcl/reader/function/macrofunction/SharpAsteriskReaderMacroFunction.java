package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.structs.arrays.BitVectorStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.reader.function.ExtendedTokenReader;
import jcl.reader.impl.Reader;
import jcl.reader.syntax.ReadExtendedToken;
import jcl.syntax.CharacterConstants;
import jcl.reader.ReadSuppressVariable;
import org.apache.commons.lang3.StringUtils;

/**
 * Implements the '#*' Lisp reader macro.
 */
public class SharpAsteriskReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.ASTERISK;

		final ExtendedTokenReader macroFunctionReader = new ExtendedTokenReader(reader);
		final ReadExtendedToken readExtendedToken = macroFunctionReader.readExtendedToken(false);
		if (ReadSuppressVariable.INSTANCE.getValue()) {
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
		}

		if (bitStringLength > numArg) {
			throw new ReaderErrorException("Bit vector is longer than specified length: #" + numArg + '*' + bitString);
		}

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
