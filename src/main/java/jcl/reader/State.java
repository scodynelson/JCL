package jcl.reader;

import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.TokenAttribute;
import jcl.reader.syntax.TokenBuilder;

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
 * <li>InitialState
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
 * <li>ErrorState
 * </ol>
 * </p>
 * For online specifications of these states, goto http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm
 * This site is the Reader Algorithm that is outlined within the CommonLisp HyperSpec (TM).
 */
public abstract class State {

	private static final int EOF = -1;

	public static State getInitialState() {
		return InitialState.INITIAL_STATE;
	}

	protected static boolean isEndOfFileCharacter(final Integer character) {
		return (character == null) || (character == EOF);
	}

	protected static String convertTokensToString(final List<TokenAttribute> tokenAttributes) {
		final StringBuilder stringBuilder = new StringBuilder();
		tokenAttributes
				.stream()
				.forEachOrdered(e -> stringBuilder.appendCodePoint(e.getToken()));
		return stringBuilder.toString();
	}

	protected static int properCaseCodePoint(final int codePoint, final AttributeType attributeType, final CaseSpec caseSpec) {

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

	protected static boolean hasAnyAttribute(final List<TokenAttribute> tokenAttributes, final AttributeType attributeType) {
		return tokenAttributes
				.stream()
				.anyMatch(e -> e.getAttributeType() == attributeType);
	}

	protected static boolean hasNoAttributes(final List<TokenAttribute> tokenAttributes, final AttributeType attributeType) {
		return tokenAttributes
				.stream()
				.noneMatch(e -> e.getAttributeType() == attributeType);
	}

	protected static Integer getTokenByAttribute(final List<TokenAttribute> tokenAttributes, final AttributeType attributeType) {
		return tokenAttributes
				.stream()
				.filter(e -> e.getAttributeType() == attributeType)
				.map(TokenAttribute::getToken)
				.findFirst()
				.orElse(null);
	}

	public abstract void process(Reader reader, TokenBuilder tokenBuilder);
}
