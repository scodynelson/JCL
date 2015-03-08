/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.arrays.BitVectorStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.List;
import java.util.stream.Collectors;

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
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.ASTERISK;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken readExtendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
		final String token = readExtendedToken.getToken();

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (readExtendedToken.isHasEscapes()) {
			throw new ReaderErrorException("Escape character appeared after #*");
		}

		final boolean isInvalidBitString = token.chars()
		                                        .map(e -> Character.getNumericValue((char) e))
		                                        .anyMatch(SharpAsteriskReaderMacroFunction::isInvalidBit);
		if (isInvalidBitString) {
			throw new ReaderErrorException("Bad Bit for Bit Vector...");
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
	private static ListStruct handleNumArg(final String token, final BigInteger numArg) {

		if (StringUtils.isEmpty(token) && (numArg.compareTo(BigInteger.ZERO) > 0)) {
			throw new ReaderErrorException("At least one bit must be supplied for non-zero #* bit-vectors.");
		}

		final int numberOfTokens = token.length();
		final int numArgInt = numArg.intValueExact();
		if (numberOfTokens > numArgInt) {
			throw new ReaderErrorException("Bit vector is longer than specified length: #" + numArg + '*' + token);
		}

		Character lastToken = null;
		if (StringUtils.isNotEmpty(token)) {
			lastToken = token.charAt(numberOfTokens - 1);
		}

		final StringBuilder bitStringBuilder = new StringBuilder(token);

		final int fillAmount = numArgInt - numberOfTokens;
		for (int i = 0; i < fillAmount; i++) {
			bitStringBuilder.append(lastToken);
		}

		final String newBitString = bitStringBuilder.toString();
		return createBitVector(newBitString);
	}

	/**
	 * Creates creates the {@link ListStruct} calling the appropriate function needed to produce the {@link
	 * BitVectorStruct} from the provided {@code token}.
	 *
	 * @param token
	 * 		the bit-vector contents used to create the {@link BitVectorStruct}
	 *
	 * @return the {@link ListStruct} calling the appropriate function needed to produce the {@link BitVectorStruct}
	 */
	private static ListStruct createBitVector(final String token) {
		final int numberOfTokens = token.length();
		final BigInteger numberOfTokensBI = BigInteger.valueOf(numberOfTokens);

		final List<LispStruct> bits = convertBitStringToBits(token);

		final SymbolStruct<?> makeArrayFnSymbol = CommonLispSymbols.MAKE_ARRAY;
		final IntegerStruct dimensions = new IntegerStruct(numberOfTokensBI);
		final SymbolStruct<?> elementTypeKeyword = GlobalPackageStruct.KEYWORD.findSymbol("ELEMENT-TYPE").getSymbolStruct();
		final ListStruct elementType = ListStruct.buildProperList(QUOTE, CommonLispSymbols.BIT);
		final SymbolStruct<?> initialContentsKeyword = GlobalPackageStruct.KEYWORD.findSymbol("INITIAL-CONTENTS").getSymbolStruct();

		final ListStruct contents = ListStruct.buildProperList(bits);
		final ListStruct initialContents = ListStruct.buildProperList(QUOTE, contents);

		return ListStruct.buildProperList(makeArrayFnSymbol, dimensions, elementTypeKeyword, elementType, initialContentsKeyword, initialContents);
	}

	/**
	 * Converts the provided {@code token} string into a list of {@link IntegerStruct} bits.
	 *
	 * @param token
	 * 		the bit-vector contents to convert into a list of {@link IntegerStruct} bits
	 *
	 * @return the list of {@link IntegerStruct} bits comprising the provided {@code token}
	 */
	private static List<LispStruct> convertBitStringToBits(final String token) {
		return token.chars()
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
