/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.BitVectorStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * Implements the '#*' Lisp reader macro.
 */
@Component
public class SharpAsteriskReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1951661697106448531L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.ASTERISK, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numberArgument) {
		assert codePoint == CharacterConstants.ASTERISK;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
		final String tokenString = extendedToken.getTokenString();

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return NullStruct.INSTANCE;
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
			return createBitVector(tokenString);
		}

		return handleNumArg(tokenString, numberArgument);
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
	private static ListStruct handleNumArg(final String tokenString, final BigInteger numberArgument) {

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
		return createBitVector(bitString);
	}

	/**
	 * Creates creates the {@link ListStruct} calling the appropriate function needed to produce the {@link
	 * BitVectorStruct} from the provided {@code token}.
	 *
	 * @param tokenString
	 * 		the bit-vector contents used to create the {@link BitVectorStruct}
	 *
	 * @return the {@link ListStruct} calling the appropriate function needed to produce the {@link BitVectorStruct}
	 */
	private static ListStruct createBitVector(final String tokenString) {
		final BigInteger numberOfTokens = BigInteger.valueOf(tokenString.length());

		final List<LispStruct> bits = convertBitStringToBits(tokenString);

		final IntegerStruct dimensions = new IntegerStruct(numberOfTokens);
		final ListStruct elementType = ListStruct.buildProperList(CommonLispSymbols.QUOTE, CommonLispSymbols.BIT);
		final ListStruct initialContents = ListStruct.buildProperList(CommonLispSymbols.QUOTE, ListStruct.buildProperList(bits));

		return ListStruct.buildProperList(CommonLispSymbols.MAKE_ARRAY,
				dimensions,
				CommonLispSymbols.ELEMENT_TYPE_KEYWORD,
				elementType,
				CommonLispSymbols.INITIAL_CONTENTS_KEYWORD,
				initialContents);
	}

	/**
	 * Converts the provided {@code token} string into a list of {@link IntegerStruct} bits.
	 *
	 * @param tokenString
	 * 		the bit-vector contents to convert into a list of {@link IntegerStruct} bits
	 *
	 * @return the list of {@link IntegerStruct} bits comprising the provided {@code token}
	 */
	private static List<LispStruct> convertBitStringToBits(final String tokenString) {
		return tokenString.chars()
		                  .map(e -> Character.getNumericValue((char) e))
		                  .mapToObj(BigInteger::valueOf)
		                  .map(IntegerStruct::new)
		                  .collect(Collectors.toList());
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
