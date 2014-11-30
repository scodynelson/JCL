/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.arrays.BitVectorStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
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
		init();
	}

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.ASTERISK, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.ASTERISK;

		final ReadExtendedToken readExtendedToken = readExtendedToken(reader);
		final String token = readExtendedToken.getToken();

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", token);
			}
			return null;
		}

		if (readExtendedToken.isHasEscapes()) {
			throw new ReaderErrorException("Escape character appeared after #*");
		}

		if (numArg == null) {
			return createBitVector(token);
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
		return createBitVector(newBitString);
	}

	/**
	 * Creates a new {@link BitVectorStruct} from the provided {@code token}.
	 *
	 * @param token
	 * 		the token used to create the {@link BitVectorStruct}
	 *
	 * @return the newly created {@link BitVectorStruct}
	 */
	private static BitVectorStruct createBitVector(final String token) {
		try {
			return new BitVectorStruct(token);
		} catch (final TypeErrorException | SimpleErrorException e) {
			throw new ReaderErrorException("Error occurred creating bit-vector.", e);
		}
	}
}
