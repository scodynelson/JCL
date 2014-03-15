package jcl.readtables.reader;

import jcl.syntax.reader.TokenAttribute;
import jcl.syntax.AttributeType;
import jcl.syntax.CaseSpec;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;

import java.util.LinkedList;

public final class StateUtils {

	private StateUtils() {
	}

	public static int properCaseCodePoint(final int codePoint, final AttributeType attributeType, final CaseSpec caseSpec) {

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

	public static boolean hasAttribute(final LinkedList<TokenAttribute> tokenAttributes, final AttributeType attributeType) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return false;
		}

		boolean hasAttribute = false;
		for (final TokenAttribute tokenAttribute : tokenAttributes) {
			if (tokenAttribute.getAttributeType() == attributeType) {
				hasAttribute = true;
				break;
			}
		}
		return hasAttribute;
	}

	public static boolean hasNoAttribute(final LinkedList<TokenAttribute> tokenAttributes, final AttributeType attributeType) {
		return !hasAttribute(tokenAttributes, attributeType);
	}

	public static boolean hasAttributes(final LinkedList<TokenAttribute> tokenAttributes, final AttributeType... attributeTypes) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return false;
		}

		final boolean[] results = new boolean[attributeTypes.length];

		for (int i = 0; i < attributeTypes.length; i++) {
			final AttributeType attributeType = attributeTypes[i];
			final boolean hasAttribute = hasAttribute(tokenAttributes, attributeType);
			results[i] = hasAttribute;
		}

		return BooleanUtils.and(results);
	}

	public static boolean hasMoreThanOneOfAttribute(final LinkedList<TokenAttribute> tokenAttributes, final AttributeType attributeType) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return false;
		}

		int attributeCount = 0;

		boolean hasMoreThanOneOfAttribute = false;
		for (final TokenAttribute tokenAttribute : tokenAttributes) {
			if (tokenAttribute.getAttributeType() == attributeType) {
				attributeCount++;
			}
			if (attributeCount > 1) {
				hasMoreThanOneOfAttribute = true;
				break;
			}
		}
		return hasMoreThanOneOfAttribute;
	}

	public static boolean hasMoreThanOneOfAttributes(final LinkedList<TokenAttribute> tokenAttributes, final AttributeType... attributeTypes) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return false;
		}

		final boolean[] results = new boolean[attributeTypes.length];

		for (int i = 0; i < attributeTypes.length; i++) {
			final AttributeType attributeType = attributeTypes[i];
			final boolean hasMoreThanOneOfAttribute = hasMoreThanOneOfAttribute(tokenAttributes, attributeType);
			results[i] = hasMoreThanOneOfAttribute;
		}

		return BooleanUtils.or(results);
	}

	public static boolean hasAttributesAndNotFirst(final AttributeType firstAttributeType, final LinkedList<TokenAttribute> tokenAttributes,
	                                               final AttributeType... attributeTypes) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return false;
		}

		final boolean[] results = new boolean[attributeTypes.length];

		for (int i = 0; i < attributeTypes.length; i++) {
			final AttributeType attributeType = attributeTypes[i];
			final boolean hasAttribute = hasAttribute(tokenAttributes, attributeType);
			results[i] = hasAttribute && (firstAttributeType != attributeType);
		}

		return BooleanUtils.or(results);
	}

	public static boolean hasAttributesAndFirstOrLast(final AttributeType firstAttributeType, final AttributeType lastAttributeType,
	                                                  final LinkedList<TokenAttribute> tokenAttributes, final AttributeType... attributeTypes) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return false;
		}

		final boolean[] results = new boolean[attributeTypes.length];

		for (int i = 0; i < attributeTypes.length; i++) {
			final AttributeType attributeType = attributeTypes[i];
			final boolean hasAttribute = hasAttribute(tokenAttributes, attributeType);
			results[i] = hasAttribute && ((firstAttributeType == attributeType) || (lastAttributeType == attributeType));
		}

		return BooleanUtils.or(results);
	}

	public static boolean areAnyTokensInvalidRegexAndUnicode(final int currentRadix, final Character.UnicodeBlock block,
	                                                         final LinkedList<TokenAttribute> tokenAttributes) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return false;
		}

		final boolean[] results = new boolean[tokenAttributes.size()];

		for (int i = 0; i < tokenAttributes.size(); i++) {
			final TokenAttribute tokenAttribute = tokenAttributes.get(i);
			final int codePoint = tokenAttribute.getToken();
			final AttributeType attributeType = tokenAttribute.getAttributeType();

			if (attributeType == AttributeType.ALPHADIGIT) {

				final boolean isDigitWithRadix = Character.digit(codePoint, currentRadix) >= 0;
				final boolean isDigitInSameBlock = Character.UnicodeBlock.of(codePoint).equals(block);
				results[i] = !(isDigitWithRadix && isDigitInSameBlock);
			}
		}

		return BooleanUtils.or(results);
	}

	public static Integer getTokenByAttribute(final LinkedList<TokenAttribute> tokenAttributes, final AttributeType attributeType) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return null;
		}

		Integer token = null;
		for (final TokenAttribute tokenAttribute : tokenAttributes) {
			if (tokenAttribute.getAttributeType() == attributeType) {
				token = tokenAttribute.getToken();
				break;
			}
		}
		return token;
	}

	public static String convertTokensToString(final LinkedList<TokenAttribute> tokenAttributes) {

		if (CollectionUtils.isEmpty(tokenAttributes)) {
			return null;
		}

		final StringBuilder stringBuilder = new StringBuilder();
		for (final TokenAttribute tokenAttribute : tokenAttributes) {
			final int token = tokenAttribute.getToken();
			stringBuilder.appendCodePoint(token);
		}
		return stringBuilder.toString();
	}
}
