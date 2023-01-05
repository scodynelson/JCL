/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import jcl.lang.BitVectorStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * Implements the '#*' Lisp reader macro.
 */
public final class SharpAsteriskReaderMacroFunction extends ReaderMacroFunctionImpl {

	private static final Pattern BIT_PATTERN = Pattern.compile("[0|1]+");

	public SharpAsteriskReaderMacroFunction() {
		super("SHARP-ASTERISK");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.ASTERISK;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken
				= ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
		final String tokenString = extendedToken.getTokenString();

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
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

		if (numberArgument == null) {
			final List<FixnumStruct> contents = getBitList(tokenString);
			final FixnumStruct size = IntegerStruct.toLispInteger(contents.size());
			return BitVectorStruct.toLispBitVector(size, contents);
		}

		return handleNumberArgument(tokenString, numberArgument);
	}

	/**
	 * Handles the processing of the number argument when parsing the provided {@code token} string into a
	 * {@link BitVectorStruct}.
	 *
	 * @param tokenString
	 * 		the bit-vector contents
	 * @param numberArgument
	 * 		the number argument passed to be used as the bit-vector length
	 *
	 * @return the properly created {@link BitVectorStruct} taking care of the proper bit-vector length
	 */
	private static BitVectorStruct handleNumberArgument(final String tokenString, final IntegerStruct numberArgument) {

		if (StringUtils.isEmpty(tokenString) && numberArgument.plusp().toJavaPBoolean()) {
			throw new ReaderErrorException("At least one bit must be supplied for non-zero #* bit-vectors.");
		}

		final int numberOfTokens = tokenString.length();
		final int numberArgumentIntValue = numberArgument.toJavaInt();
		if (numberOfTokens > numberArgumentIntValue) {
			final String message = "Bit vector is longer than specified length: #" + numberArgument + '*' + tokenString;
			throw new ReaderErrorException(message);
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
		final List<FixnumStruct> contents = getBitList(bitString);
		final FixnumStruct size = IntegerStruct.toLispInteger(contents.size());
		return BitVectorStruct.toLispBitVector(size, contents);
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

	/**
	 * Gets a list of {@link IntegerStruct}s from the provided {@link String} value.
	 *
	 * @param bitString
	 * 		the Java string to convert to a list of {@link IntegerStruct}s
	 *
	 * @return a list of {@link IntegerStruct}s from the provided {@link String} value
	 */
	private static List<FixnumStruct> getBitList(final String bitString) {
		if (!bitString.isEmpty() && !BIT_PATTERN.matcher(bitString).matches()) {
			throw new TypeErrorException(
					"Input contains characters not of type " + CommonLispSymbols.BIT + ": " + bitString + '.');
		}

		final List<FixnumStruct> bitList = new ArrayList<>(bitString.length());
		for (final char character : bitString.toCharArray()) {
			if (character == '0') {
				bitList.add(IntegerStruct.ZERO);
			} else if (character == '1') {
				bitList.add(IntegerStruct.ONE);
			}
		}
		return bitList;
	}
}
