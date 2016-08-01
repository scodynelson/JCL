/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.BitVectorStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.list.NILStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * Implements the '#*' Lisp reader macro.
 */
@Component
public class SharpAsteriskReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.ASTERISK, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.ASTERISK;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
		final String tokenString = extendedToken.getTokenString();

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (extendedToken.isHasEscapes()) {
			throw new ReaderErrorException("Escape character appeared after #*");
		}

		final boolean isInvalidBitString = tokenString.chars()
		                                              .map(e -> Character.getNumericValue((char) e))
		                                              .anyMatch(SharpAsteriskReaderMacroFunction::isInvalidBit);
		if (isInvalidBitString) {
			throw new ReaderErrorException("Bad Bit for Bit Vector...");
		}

		if (!numberArgument.isPresent()) {
			return LispStructFactory.toBitVector(tokenString);
		}

		final BigInteger numberArgumentValue = numberArgument.get();
		return handleNumberArgument(tokenString, numberArgumentValue);
	}

	/**
	 * Handles the processing of the number argument when parsing the provided {@code token} string into a {@link
	 * BitVectorStruct}.
	 *
	 * @param tokenString
	 * 		the bit-vector contents
	 * @param numberArgument
	 * 		the number argument passed to be used as the bit-vector length
	 *
	 * @return the properly created {@link BitVectorStruct} taking care of the proper bit-vector length
	 */
	private static BitVectorStruct handleNumberArgument(final String tokenString, final BigInteger numberArgument) {

		if (StringUtils.isEmpty(tokenString) && (numberArgument.compareTo(BigInteger.ZERO) > 0)) {
			throw new ReaderErrorException("At least one bit must be supplied for non-zero #* bit-vectors.");
		}

		final int numberOfTokens = tokenString.length();
		final int numberArgumentIntValue = numberArgument.intValueExact();
		if (numberOfTokens > numberArgumentIntValue) {
			throw new ReaderErrorException("Bit vector is longer than specified length: #" + numberArgument + '*' + tokenString);
		}

		Character lastToken = null;
		if (StringUtils.isNotEmpty(tokenString)) {
			lastToken = tokenString.charAt(numberOfTokens - 1);
		}

		final StringBuilder bitStringBuilder = new StringBuilder(tokenString);

		final int fillAmount = numberArgumentIntValue - numberOfTokens;
		for (int i = 0; i < fillAmount; i++) {
			bitStringBuilder.append(lastToken);
		}

		final String bitString = bitStringBuilder.toString();
		return LispStructFactory.toBitVector(bitString);
	}

	/**
	 * Determines whether or not the provided {@code value} is a valid bit (aka. a '0' or a '1').
	 *
	 * @param value
	 * 		the value to check whether or not is a valid bit
	 *
	 * @return true if the provided {@code value} is an invalid bit; false otherwise
	 */
	private static boolean isInvalidBit(final int value) {
		return (value != 0) && (value != 1);
	}
}
