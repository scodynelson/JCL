/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;

import java.util.List;

/**
 * This interface defines a set of anonymous classes that comprise the states of the Reader state machine as defined in
 * CLtL: Ch 22.1.1 pp 511-515. These states are active objects having a single {@code process} method. Each state
 * returns a State object that is the next state to process. The current Reader instance is passed to each State. The
 * Reader instance contains a reference to the current input Stream. A state processes according to the specification
 * and returns the next state. The states in CLtL are numbered. The following is a correspondence list between the
 * numbered states and the named states in this interface.
 * <p>
 * <ol start=0>
 * <li>ReadState
 * <li>IllegalCharState
 * <li>WhitespaceState
 * <li>MacroCharacterState
 * <li>SingleEscapeState
 * <li>MultipleEscapeState
 * <li>ConstituentState
 * <li>EvenMultiEscapeState
 * <li>OddMultiEscapeState
 * <li>TokenAccumulatedState
 * </ol>
 * </p>
 * For online specifications of these states, goto http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm
 * This site is the Reader Algorithm that is outlined within the CommonLisp HyperSpec (TM).
 */
@FunctionalInterface
interface State {

	/**
	 * Abstract method to be implemented by all child State objects to handle their respective reader processing
	 * portion.
	 *
	 * @param readerStateMediator
	 * @param reader
	 * 		the JCL {@link Reader} instance to use for reading lisp tokens.
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to build the resulting lisp token and house token parsing information throughout
	 * 		the read process
	 */
	void process(ReaderStateMediator readerStateMediator, Reader reader, TokenBuilder tokenBuilder);

	/**
	 * Determines if the provided {@code codePoint} is either null or equal to {@link CharacterConstants#EOF}.
	 *
	 * @param codePoint
	 * 		the value to determine is either null or equal to {@link CharacterConstants#EOF}
	 *
	 * @return true if the provided {@code codePoint} is an End-Of-File character
	 */
	static boolean isEndOfFileCharacter(final Integer codePoint) {
		return (codePoint == null) || (codePoint == CharacterConstants.EOF);
	}

	/**
	 * Handles End-of-File characters based on the {@link TokenBuilder#isEofErrorP} value inside the provided {@link
	 * TokenBuilder}.
	 *
	 * @param tokenBuilder
	 * 		the {@link TokenBuilder} used to determine the handling of End-of-File characters
	 * @param stateEofOccurred
	 * 		the State the End-of-File character occurred
	 */
	static void handleEndOfFile(final TokenBuilder tokenBuilder, final String stateEofOccurred) {
		if (tokenBuilder.isEofErrorP()) {
			throw new ReaderErrorException("End-of-File encountered in " + stateEofOccurred + '.');
		} else {
			tokenBuilder.setReturnToken(null);
		}
	}

	/**
	 * Converts the provided list of {@link TokenAttribute}s to a {@link String}.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s to convert to a {@link String}
	 *
	 * @return the {@link String} produced from the list of {@link TokenAttribute}s
	 */
	static String convertTokensToString(final List<TokenAttribute> tokenAttributes) {
		final StringBuilder stringBuilder = new StringBuilder();
		tokenAttributes
				.stream()
				.mapToInt(TokenAttribute::getToken)
				.forEachOrdered(stringBuilder::appendCodePoint);
		return stringBuilder.toString();
	}

	/**
	 * Determines and returns the proper code point value based from the provided {@code codePoint} and using the
	 * provided {@code attributeType} and {@code caseSpec} properties.
	 *
	 * @param codePoint
	 * 		the code point value to properly case
	 * @param attributeType
	 * 		the {@link AttributeType} of the code point value used in determining the proper case value
	 * @param caseSpec
	 * 		the current readtable case used in determines the proper case value
	 *
	 * @return the proper code point value based from the provided {@code codePoint}
	 */
	static int properCaseCodePoint(final int codePoint, final AttributeType attributeType, final CaseSpec caseSpec) {

		int properCaseCodePoint = codePoint;
		if (Character.isBmpCodePoint(codePoint)) {
			if ((caseSpec == CaseSpec.UPCASE) && ((attributeType == AttributeType.ALPHADIGIT) || (attributeType == AttributeType.EXPONENTMARKER))) {
				properCaseCodePoint = Character.toUpperCase(codePoint);
			} else if (caseSpec == CaseSpec.DOWNCASE) {
				properCaseCodePoint = Character.toLowerCase(codePoint);
			} else if (caseSpec == CaseSpec.INVERT) {
				if (Character.isUpperCase(codePoint)) {
					properCaseCodePoint = Character.toLowerCase(codePoint);
				} else {
					properCaseCodePoint = Character.toUpperCase(codePoint);
				}
			}
		}
		return properCaseCodePoint;
	}

	/**
	 * Determines if the provided list of {@link TokenAttribute}s contains at least one token with an {@link
	 * AttributeType} equal to the provided {@code attributeType} value.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s containing the current tokens
	 * @param attributeType
	 * 		the {@link AttributeType} value to check for existence in the {@code tokenAttributes} list
	 *
	 * @return if the provided list of {@link TokenAttribute}s contains at least one token with an {@link AttributeType}
	 * equal to the provided {@code attributeType} value.
	 */
	static boolean hasAnyAttribute(final List<TokenAttribute> tokenAttributes, final AttributeType attributeType) {
		return tokenAttributes
				.stream()
				.anyMatch(e -> e.getAttributeType() == attributeType);
	}

	/**
	 * Determines if the provided list of {@link TokenAttribute}s contains no tokens with an {@link AttributeType}
	 * equal to the provided {@code attributeType} value.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s containing the current tokens
	 * @param attributeType
	 * 		the {@link AttributeType} value to check for existence in the {@code tokenAttributes} list
	 *
	 * @return if the provided list of {@link TokenAttribute}s contains no tokens with an {@link AttributeType} equal to
	 * the provided {@code attributeType} value.
	 */
	static boolean hasNoAttributes(final List<TokenAttribute> tokenAttributes, final AttributeType attributeType) {
		return tokenAttributes
				.stream()
				.noneMatch(e -> e.getAttributeType() == attributeType);
	}

	/**
	 * Gets the first occurrence of a token with an {@link AttributeType} equal to the provided {@code attributeType}
	 * value in the provided list of {@link TokenAttribute}s. If no tokens have an {@link AttributeType} that matches
	 * the provided {@code attributeType} value, null will be returned.
	 *
	 * @param tokenAttributes
	 * 		the list of {@link TokenAttribute}s containing the current tokens
	 * @param attributeType
	 * 		the {@link AttributeType} value used to locate the first matching token in the {@code tokenAttributes} list
	 *
	 * @return the first occurrence of a token with an {@link AttributeType} equal to the provided {@code attributeType}
	 * value in the provided list of {@link TokenAttribute}s or null if no such token can be found
	 */
	static Integer getTokenByAttribute(final List<TokenAttribute> tokenAttributes, final AttributeType attributeType) {
		return tokenAttributes
				.stream()
				.filter(e -> e.getAttributeType() == attributeType)
				.map(TokenAttribute::getToken)
				.findFirst()
				.orElse(null);
	}
}
