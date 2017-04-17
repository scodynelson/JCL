package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.ComplexStringStructImpl;
import jcl.lang.internal.SimpleStringStructImpl;
import jcl.lang.statics.CharacterConstants;
import jcl.type.CharacterType;
import jcl.type.LispType;

/**
 * The {@link StringStruct} is the object representation of a Lisp 'string' type.
 */
public interface StringStruct extends VectorStruct {

	/**
	 * Constant representing an empty StringStruct.
	 */
	StringStruct EMPTY_STRING = new SimpleStringStructImpl("");

	/**
	 * Retrieves the {@link CharacterStruct} at the provided {@link IntegerStruct} index from the structure.
	 *
	 * @param index
	 * 		the position of the {@link CharacterStruct} to retrieve
	 *
	 * @return the {@link CharacterStruct} at the provided index
	 */
	CharacterStruct char_(final IntegerStruct index);

	/**
	 * Sets the {@link CharacterStruct} at the provided {@link IntegerStruct} index within the structure to the provided
	 * new {@link CharacterStruct} element.
	 *
	 * @param newElement
	 * 		the new {@link CharacterStruct} to set at the provided index
	 * @param index
	 * 		the position to modify and set as the provided new element value
	 *
	 * @return the new {@link CharacterStruct} element
	 */
	CharacterStruct setfChar(final CharacterStruct newElement, final IntegerStruct index);

	/**
	 * Retrieves the {@link CharacterStruct} at the provided {@link IntegerStruct} index from the structure, only if the
	 * structure is a 'simple' string.
	 *
	 * @param index
	 * 		the position of the {@link CharacterStruct} to retrieve
	 *
	 * @return the {@link CharacterStruct} at the provided index
	 */
	CharacterStruct schar(final IntegerStruct index);

	/**
	 * Sets the {@link CharacterStruct} at the provided {@link IntegerStruct} index within the structure to the provided
	 * new {@link CharacterStruct} element, only if the structure is a 'simple' string.
	 *
	 * @param newElement
	 * 		the new {@link CharacterStruct} to set at the provided index
	 * @param index
	 * 		the position to modify and set as the provided new element value
	 *
	 * @return the new {@link CharacterStruct} element
	 */
	CharacterStruct setfSchar(final CharacterStruct newElement, final IntegerStruct index);

	/**
	 * Returns a new string with the contents upper-cased according to the provided {@link StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return a new string with the contents upper-cased
	 */
	StringStruct stringUpcase(final StringIntervalOpContext context);

	/**
	 * Returns a new string with the contents lower-cased according to the provided {@link StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return a new string with the contents lower-cased
	 */
	StringStruct stringDowncase(final StringIntervalOpContext context);

	/**
	 * Returns a new string with the contents capitalized according to the provided {@link StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return a new string with the contents capitalized
	 */
	StringStruct stringCapitalize(final StringIntervalOpContext context);

	/**
	 * Destructively modifies this string with the contents upper-cased according to the provided {@link
	 * StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return this string with the contents upper-cased
	 */
	StringStruct nStringUpcase(final StringIntervalOpContext context);

	/**
	 * Destructively modifies this string with the contents lower-cased according to the provided {@link
	 * StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return this string with the contents lower-cased
	 */
	StringStruct nStringDowncase(final StringIntervalOpContext context);

	/**
	 * Destructively modifies this string with the contents capitalized according to the provided {@link
	 * StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return this string with the contents capitalized
	 */
	StringStruct nStringCapitalize(final StringIntervalOpContext context);

	/**
	 * Returns a new string with the characters in the provided character-bag trimmed from the beginning and end of the
	 * string.
	 *
	 * @param characterBag
	 * 		the bag of characters to trim from the string
	 *
	 * @return and new string with the characters in the provided character-bag trimmed from the beginning and end of
	 * the string
	 */
	StringStruct stringTrim(final SequenceStruct characterBag);

	/**
	 * Returns a new string with the characters in the provided character-bag trimmed from the beginning of the string.
	 *
	 * @param characterBag
	 * 		the bag of characters to trim from the string
	 *
	 * @return and new string with the characters in the provided character-bag trimmed from the beginning of the string
	 */
	StringStruct stringLeftTrim(final SequenceStruct characterBag);

	/**
	 * Returns a new string with the characters in the provided character-bag trimmed from the end of the string.
	 *
	 * @param characterBag
	 * 		the bag of characters to trim from the string
	 *
	 * @return and new string with the characters in the provided character-bag trimmed from the end of the string
	 */
	StringStruct stringRightTrim(final SequenceStruct characterBag);

	/**
	 * Determines equality of strings according to the provided {@link StringEqualityContext}. Case is accounted for.
	 *
	 * @param context
	 * 		the equality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#T} if the strings are equal; {@link BooleanStruct#NIL} otherwise
	 */
	BooleanStruct stringEqual(final StringEqualityContext context);

	/**
	 * Determines inequality of strings according to the provided {@link StringEqualityContext}. Case is accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the strings are not equal; an {@link IntegerStruct} mismatch index where the
	 * strings differ otherwise
	 */
	LispStruct stringNotEqual(final StringEqualityContext context);

	/**
	 * Determines less-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is less-than the second; an {@link IntegerStruct} mismatch
	 * index where the strings differ otherwise
	 */
	LispStruct stringLessThan(final StringEqualityContext context);

	/**
	 * Determines greater-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is greater-than the second; an {@link IntegerStruct}
	 * mismatch index where the strings differ otherwise
	 */
	LispStruct stringGreaterThan(final StringEqualityContext context);

	/**
	 * Determines less-than-or-equal-to inequality of strings according to the provided {@link StringEqualityContext}.
	 * Case is accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is less-than-or-equal-to the second; an {@link
	 * IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringLessThanOrEqualTo(final StringEqualityContext context);

	/**
	 * Determines greater-than-or-equal-to inequality of strings according to the provided {@link
	 * StringEqualityContext}. Case is accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is greater-than-or-equal-to the second; an {@link
	 * IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringGreaterThanOrEqualTo(final StringEqualityContext context);

	/**
	 * Determines equality of strings according to the provided {@link StringEqualityContext}. Case is ignored.
	 *
	 * @param context
	 * 		the equality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#T} if the strings are equal; {@link BooleanStruct#NIL} otherwise
	 */
	BooleanStruct stringEqualIgnoreCase(final StringEqualityContext context);

	/**
	 * Determines inequality of strings according to the provided {@link StringEqualityContext}. Case is ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the strings are not equal; an {@link IntegerStruct} mismatch index where the
	 * strings differ otherwise
	 */
	LispStruct stringNotEqualIgnoreCase(final StringEqualityContext context);

	/**
	 * Determines less-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is less-than the second; an {@link IntegerStruct} mismatch
	 * index where the strings differ otherwise
	 */
	LispStruct stringLessThanIgnoreCase(final StringEqualityContext context);

	/**
	 * Determines greater-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is greater-than the second; an {@link IntegerStruct}
	 * mismatch index where the strings differ otherwise
	 */
	LispStruct stringGreaterThanIgnoreCase(final StringEqualityContext context);

	/**
	 * Determines less-than-or-equal-to inequality of strings according to the provided {@link StringEqualityContext}.
	 * Case is ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is less-than-or-equal-to the second; an {@link
	 * IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringLessThanOrEqualToIgnoreCase(final StringEqualityContext context);

	/**
	 * Determines greater-than-or-equal-to inequality of strings according to the provided {@link
	 * StringEqualityContext}. Case is ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return {@link BooleanStruct#NIL} if the first string is greater-than-or-equal-to the second; an {@link
	 * IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringGreaterThanOrEqualToIgnoreCase(final StringEqualityContext context);

	/**
	 * Returns whether or not the String is a 'simple' string.
	 *
	 * @return true if the String is a 'simple' string; false otherwise
	 */
	BooleanStruct isSimpleString();

	/**
	 * Returns the {@link String} representation of the StringStruct.
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	String toJavaString();

	/**
	 * Returns the {@link String} representation of the StringStruct, ignoring the fill-pointer attribute accordingly
	 * based on the provided {@code ignoreFillPointer} value.
	 *
	 * @param ignoreFillPointer
	 * 		whether or not to ignore the fill-pointer value when constructing the resulting {@link String}
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	String toJavaString(final boolean ignoreFillPointer);

	/**
	 * Returns a new empty StringStruct with no contents.
	 *
	 * @return a new empty StringStruct with no contents
	 */
	static StringStruct emptyString() {
		return EMPTY_STRING;
	}

	/**
	 * Returns a new StringStruct representation of the provided {@link String}.
	 *
	 * @param str
	 * 		the {@link String} to represent as a StringStruct
	 *
	 * @return a new StringStruct representation of the provided {@link String}
	 */
	static StringStruct toLispString(final String str) {
		return new SimpleStringStructImpl(str);
	}

	/**
	 * Returns a new {@link StringStruct.Builder} to be used in constructing a new StringStruct.
	 *
	 * @param size
	 * 		the expected size of the resulting StringStruct from the builder operation
	 *
	 * @return a new {@link StringStruct.Builder} to be used in constructing a new StringStruct
	 */
	static StringStruct.Builder builder(final IntegerStruct size) {
		return new StringStruct.Builder(size);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	StringStruct adjustArray(final AdjustArrayContext context);

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	StringStruct reverse();

	@Override
	StringStruct nReverse();

	/**
	 * Builder factory for creating {@link StringStruct} objects.
	 */
	final class Builder extends ArrayStruct.AbstractBuilder<StringStruct, CharacterType, CharacterStruct> {

		/**
		 * The size of the resulting {@link StringStruct}.
		 */
		private final IntegerStruct size;

		/**
		 * The fill-pointer value of the resulting {@link StringStruct}.
		 */
		private IntegerStruct fillPointer;

		/**
		 * Private constructor.
		 *
		 * @param size
		 * 		the expected size of the resulting {@link StringStruct}
		 */
		private Builder(final IntegerStruct size) {
			super(CharacterType.INSTANCE, CharacterConstants.NULL_CHAR);
			this.size = size;
		}

		@Override
		public StringStruct.Builder elementType(final CharacterType elementType) {
			this.elementType = elementType;
			return this;
		}

		@Override
		public StringStruct.Builder initialElement(final CharacterStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		@Override
		public StringStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		@Override
		public StringStruct.Builder adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		@Override
		public StringStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		@Override
		public StringStruct.Builder displacedTo(final ArrayStruct displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		@Override
		public StringStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		@Override
		public StringStruct build() {
			final int sizeInt = size.intValue();
			final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable.booleanValue();
			final Integer fillPointerInt = (fillPointer == null) ? null : fillPointer.intValue();

			if (displacedTo != null) {
				final LispType displacedToType = displacedTo.arrayElementType();
				if (displacedToType.isNotOfType(upgradedET)) {
					throw new TypeErrorException(
							"Provided displaced to " + displacedTo + " is not an array with a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}
				// Check displaced index
				displacedTo.rowMajorAref(displacedIndexOffset);

				return new ComplexStringStructImpl(sizeInt,
				                                   (CharacterType) upgradedET,
				                                   displacedTo,
				                                   displacedIndexOffset.intValue(),
				                                   adjustableBoolean,
				                                   fillPointerInt);
			}

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (initialElementType.isNotOfType(upgradedET)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<CharacterStruct> validContents
						= ArrayStruct.getValidContents(Collections.singletonList(sizeInt),
						                               upgradedET,
						                               initialContents);

				final StringBuilder contents = validContents.stream()
				                                            .mapToInt(CharacterStruct::getCodePoint)
				                                            .collect(StringBuilder::new,
				                                                     StringBuilder::appendCodePoint,
				                                                     StringBuilder::append);
				if ((fillPointerInt == null) && !adjustableBoolean) {
					return new SimpleStringStructImpl(sizeInt,
					                                  CharacterType.INSTANCE,
					                                  contents);
				}
				return new ComplexStringStructImpl(sizeInt,
				                                   CharacterType.INSTANCE,
				                                   contents,
				                                   adjustableBoolean,
				                                   fillPointerInt);
			}

			final LispType initialElementType = initialElement.getType();
			if (initialElementType.isNotOfType(upgradedET)) {
				// NOTE: This should never get hit due to the implementation of array-upgraded-element-type
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}

			final StringBuilder contents = Stream.generate(() -> initialElement)
			                                     .limit(sizeInt)
			                                     .mapToInt(CharacterStruct::getCodePoint)
			                                     .collect(StringBuilder::new,
			                                              StringBuilder::appendCodePoint,
			                                              StringBuilder::append);
			if ((fillPointerInt == null) && !adjustableBoolean) {
				return new SimpleStringStructImpl(sizeInt,
				                                  CharacterType.INSTANCE,
				                                  contents);
			}
			return new ComplexStringStructImpl(sizeInt,
			                                   CharacterType.INSTANCE,
			                                   contents,
			                                   adjustableBoolean,
			                                   fillPointerInt);
		}
	}
}
