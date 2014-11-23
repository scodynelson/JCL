/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.structs.arrays.BitVectorStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.structs.symbols.variables.Variable;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

/**
 * Implements the '#*' Lisp reader macro.
 */
public final class SharpAsteriskReaderMacroFunction extends ExtendedTokenReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpAsteriskReaderMacroFunction INSTANCE = new SharpAsteriskReaderMacroFunction();

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpAsteriskReaderMacroFunction.class);

	/**
	 * Private constructor.
	 */
	private SharpAsteriskReaderMacroFunction() {
		super(false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.ASTERISK;

		final ReadExtendedToken readExtendedToken = readExtendedToken(reader);
		final String token = readExtendedToken.getToken();

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			LOGGER.debug("{} suppressed.", token);
			return null;
		}

		if (readExtendedToken.isHasEscapes()) {
			throw new ReaderErrorException("Escape character appeared after #*");
		}

		if (numArg == null) {
			try {
				return new BitVectorStruct(token);
			} catch (final TypeErrorException | SimpleErrorException e) {
				throw new ReaderErrorException("Error occurred creating bit-vector.", e);
			}
		}

		return handleNumArg(token, numArg);
	}

	/**
	 * Handles the processing of the number argument when parsing the provided {@code token} string into a {@link
	 * BitVectorStruct}.
	 *
	 * @param token
	 * 		the bit-vector contents
	 * @param numArg
	 * 		the number argument passed to be used as the bit-vector length
	 *
	 * @return the properly created {@link BitVectorStruct} taking care of the proper bit-vector length
	 */
	private static LispStruct handleNumArg(final String token, final BigInteger numArg) {

		if (StringUtils.isEmpty(token)) {
			throw new ReaderErrorException("At least one bit must be supplied for non-zero #* bit-vectors.");
		}

		final int bitStringLength = token.length();

		final int numArgInt = numArg.intValueExact();
		if (bitStringLength > numArgInt) {
			throw new ReaderErrorException("Bit vector is longer than specified length: #" + numArg + '*' + token);
		}

		final char lastChar = token.charAt(bitStringLength - 1);

		final StringBuilder bitStringBuilder = new StringBuilder(token);

		final int fillAmount = numArgInt - bitStringLength;
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
