package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.StringStructImpl;
import jcl.lang.statics.CharacterConstants;
import jcl.type.BaseCharType;
import jcl.type.BaseStringType;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.SimpleBaseStringType;
import jcl.type.SimpleStringType;
import jcl.type.StringType;

/**
 * The {@link StringStruct} is the object representation of a Lisp 'string' type.
 */
public interface StringStruct extends VectorStruct {

	/**
	 * Constant representing an empty StringStruct.
	 */
	StringStructImpl EMPTY_STRING = new StringStructImpl(SimpleStringType.INSTANCE,
	                                                     0,
	                                                     CharacterType.INSTANCE,
	                                                     new StringBuilder(),
	                                                     false,
	                                                     null);

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
	 * structure is a {@link SimpleStringType} structure.
	 *
	 * @param index
	 * 		the position of the {@link CharacterStruct} to retrieve
	 *
	 * @return the {@link CharacterStruct} at the provided index
	 */
	default CharacterStruct schar(final IntegerStruct index) {
		final LispType type = getType();
		if (SimpleStringType.INSTANCE.equals(type)) {
			return char_(index);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleStringType.INSTANCE + '.');
	}

	/**
	 * Sets the {@link CharacterStruct} at the provided {@link IntegerStruct} index within the structure to the provided
	 * new {@link CharacterStruct} element, only if the structure is a {@link SimpleStringType} structure.
	 *
	 * @param newElement
	 * 		the new {@link CharacterStruct} to set at the provided index
	 * @param index
	 * 		the position to modify and set as the provided new element value
	 *
	 * @return the new {@link CharacterStruct} element
	 */
	default CharacterStruct setfSchar(final CharacterStruct newElement, final IntegerStruct index) {
		final LispType type = getType();
		if (SimpleStringType.INSTANCE.equals(type)) {
			return setfChar(newElement, index);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleStringType.INSTANCE + '.');
	}

	/**
	 * Context argument class for string functions that require interval arguments to produce the intended result.
	 */
	final class StringIntervalOpContext {

		/**
		 * Builder factory for {@link StringIntervalOpContext}.
		 */
		public static final class Builder {

			/**
			 * Starting index to perform a string operation.
			 */
			private IntegerStruct start;

			/**
			 * Ending index to perform a string operation.
			 */
			private IntegerStruct end;

			/**
			 * Private constructor.
			 */
			private Builder() {
			}

			/**
			 * Builder method for setting {@link #start} value.
			 *
			 * @param start
			 * 		new value for {@link #start}
			 *
			 * @return the current builder instance
			 */
			public StringIntervalOpContext.Builder start(final IntegerStruct start) {
				this.start = start;
				return this;
			}

			/**
			 * Builder method for setting {@link #end} value.
			 *
			 * @param end
			 * 		new value for {@link #end}
			 *
			 * @return the current builder instance
			 */
			public StringIntervalOpContext.Builder end(final IntegerStruct end) {
				this.end = end;
				return this;
			}

			/**
			 * Builder method for constructing a new {@link StringIntervalOpContext} from the current {@link #start} and
			 * {@link #end} values.
			 *
			 * @return a new {@link StringIntervalOpContext} from the current {@link #start} and {@link #end} values
			 */
			public StringIntervalOpContext build() {
				return new StringIntervalOpContext(start, end);
			}
		}

		/**
		 * Starting index to perform a string operation.
		 */
		private final IntegerStruct start;

		/**
		 * Ending index to perform a string operation.
		 */
		private final IntegerStruct end;

		/**
		 * Private constructor initializing {@link #start} and {@link #end} values.
		 *
		 * @param start
		 * 		initial value for {@link #start}
		 * @param end
		 * 		initial value for {@link #end}
		 */
		private StringIntervalOpContext(final IntegerStruct start, final IntegerStruct end) {
			this.start = start;
			this.end = end;
		}

		/**
		 * Getter for {@link #start} value.
		 *
		 * @return {@link #start} value
		 */
		public IntegerStruct getStart() {
			return start;
		}

		/**
		 * Getter for {@link #end} value.
		 *
		 * @return {@link #end} value
		 */
		public IntegerStruct getEnd() {
			return end;
		}

		/**
		 * Factory method for retrieving a new {@link  StringIntervalOpContext.Builder} object.
		 *
		 * @return a new {@link  StringIntervalOpContext.Builder} object
		 */
		public static StringIntervalOpContext.Builder builder() {
			return new StringIntervalOpContext.Builder();
		}
	}

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
	 * Context argument class for string functions that require equality checking arguments to produce the intended
	 * result.
	 */
	final class StringEqualityContext {

		/**
		 * Builder factory for {@link StringEqualityContext}.
		 */
		public static final class Builder {

			/**
			 * The {@link StringStruct} to compare against.
			 */
			private final StringStruct struct;

			/**
			 * The {@link StringIntervalOpContext.Builder} to build the intervals for the first {@link StringStruct} in
			 * the equality operation.
			 */
			private final StringIntervalOpContext.Builder intervalOpContext1 = StringIntervalOpContext.builder();

			/**
			 * The {@link StringIntervalOpContext.Builder} to build the intervals for the second {@link StringStruct} in
			 * the equality operation.
			 */
			private final StringIntervalOpContext.Builder intervalOpContext2 = StringIntervalOpContext.builder();

			/**
			 * Private constructor.
			 *
			 * @param struct
			 * 		the {@link StringStruct} to compare against
			 */
			private Builder(final StringStruct struct) {
				this.struct = struct;
			}

			/**
			 * Builder method for setting {@link StringIntervalOpContext.Builder#start} value for {@link
			 * #intervalOpContext1}.
			 *
			 * @param start1
			 * 		new value for {@link StringIntervalOpContext.Builder#start} for {@link #intervalOpContext1}
			 *
			 * @return the current builder instance
			 */
			public StringEqualityContext.Builder start1(final IntegerStruct start1) {
				intervalOpContext1.start(start1);
				return this;
			}

			/**
			 * Builder method for setting {@link StringIntervalOpContext.Builder#end} value for {@link
			 * #intervalOpContext1}.
			 *
			 * @param end1
			 * 		new value for {@link StringIntervalOpContext.Builder#end} for {@link #intervalOpContext1}
			 *
			 * @return the current builder instance
			 */
			public StringEqualityContext.Builder end1(final IntegerStruct end1) {
				intervalOpContext1.end(end1);
				return this;
			}

			/**
			 * Builder method for setting {@link StringIntervalOpContext.Builder#start} value for {@link
			 * #intervalOpContext2}.
			 *
			 * @param start2
			 * 		new value for {@link StringIntervalOpContext.Builder#start} for {@link #intervalOpContext1}
			 *
			 * @return the current builder instance
			 */
			public StringEqualityContext.Builder start2(final IntegerStruct start2) {
				intervalOpContext2.start(start2);
				return this;
			}

			/**
			 * Builder method for setting {@link StringIntervalOpContext.Builder#end} value for {@link
			 * #intervalOpContext2}.
			 *
			 * @param end2
			 * 		new value for {@link StringIntervalOpContext.Builder#end} for {@link #intervalOpContext2}
			 *
			 * @return the current builder instance
			 */
			public StringEqualityContext.Builder end2(final IntegerStruct end2) {
				intervalOpContext2.end(end2);
				return this;
			}

			/**
			 * Builder method for constructing a new {@link StringEqualityContext} from the current {@link #struct},
			 * {@link #intervalOpContext1}, and {@link #intervalOpContext2} values.
			 *
			 * @return a new {@link StringIntervalOpContext} from the current {@link #struct}, {@link
			 * #intervalOpContext1}, and {@link #intervalOpContext2} values
			 */
			public StringEqualityContext build() {
				return new StringEqualityContext(struct,
				                                 intervalOpContext1.build(),
				                                 intervalOpContext2.build());
			}
		}

		/**
		 * The {@link StringStruct} to compare against.
		 */
		private final StringStruct struct;

		/**
		 * The {@link StringIntervalOpContext} containing the interval boundaries for the first {@link StringStruct} in
		 * the equality operation.
		 */
		private final StringIntervalOpContext context1;

		/**
		 * The {@link StringIntervalOpContext} containing the interval boundaries for the second {@link StringStruct} in
		 * the equality operation.
		 */
		private final StringIntervalOpContext context2;

		/**
		 * Private constructor for initializing the {@link #struct}, {@link #context1}, and {@link #context2} values.
		 *
		 * @param struct
		 * 		the {@link StringStruct} to compare against
		 * @param context1
		 * 		{@link StringIntervalOpContext} containing the interval boundaries for the first {@link StringStruct} in
		 * 		the equality operation
		 * @param context2
		 * 		{@link StringIntervalOpContext} containing the interval boundaries for the second {@link StringStruct}
		 * 		in the equality operation
		 */
		private StringEqualityContext(final StringStruct struct,
		                              final StringIntervalOpContext context1,
		                              final StringIntervalOpContext context2) {
			this.struct = struct;
			this.context1 = context1;
			this.context2 = context2;
		}

		/**
		 * Getter for {@link #struct} value.
		 *
		 * @return {@link #struct} value
		 */
		public StringStruct getStruct() {
			return struct;
		}

		/**
		 * Getter for {@link #context1} value.
		 *
		 * @return {@link #context1} value
		 */
		public StringIntervalOpContext getContext1() {
			return context1;
		}

		/**
		 * Getter for {@link #context2} value.
		 *
		 * @return {@link #context2} value
		 */
		public StringIntervalOpContext getContext2() {
			return context2;
		}

		/**
		 * Factory method for retrieving a new {@link  StringEqualityContext.Builder} object.
		 *
		 * @param struct
		 * 		the {@link StringStruct} to compare against
		 *
		 * @return a new {@link  StringEqualityContext.Builder} object
		 */
		public static StringEqualityContext.Builder builder(final StringStruct struct) {
			return new StringEqualityContext.Builder(struct);
		}
	}

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
	 * Returns the {@link String} representation of the StringStruct.
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	default String toJavaString() {
		// TODO: right now this ignores fill-pointer by default. Should it or should it not??
		return toJavaString(true);
	}

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
		return new StringStructImpl(SimpleStringType.INSTANCE,
		                            str.length(),
		                            CharacterType.INSTANCE,
		                            new StringBuilder(str),
		                            false,
		                            null);
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

				final StringType stringType = getStringType(adjustableBoolean, fillPointerInt, upgradedET);
				return new StringStructImpl(stringType,
				                            sizeInt,
				                            upgradedET,
				                            displacedTo,
				                            displacedIndexOffset.intValue(),
				                            adjustableBoolean,
				                            fillPointerInt);
			}
			final StringType stringType = getStringType(adjustableBoolean, fillPointerInt, upgradedET);

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
				return new StringStructImpl(stringType,
				                            sizeInt,
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
			return new StringStructImpl(stringType,
			                            sizeInt,
			                            CharacterType.INSTANCE,
			                            contents,
			                            adjustableBoolean,
			                            fillPointerInt);
		}

		/**
		 * Gets the string type from the provided isAdjustable, fillPointer, and elementType values.
		 *
		 * @param isAdjustable
		 * 		whether or not the string is adjustable
		 * @param fillPointer
		 * 		the string fillPointer
		 * @param elementType
		 * 		the string elementType
		 *
		 * @return the matching string type for the provided isAdjustable, fillPointer, and elementType values
		 */
		public static StringType getStringType(final boolean isAdjustable,
		                                       final Integer fillPointer,
		                                       final LispType elementType) {
			// TODO: Refactor this out somewhere shared??
			if (isAdjustable || (fillPointer != null)) {
				return (elementType instanceof BaseCharType)
				       ? BaseStringType.INSTANCE
				       : StringType.INSTANCE;
			} else {
				return (elementType instanceof BaseCharType)
				       ? SimpleBaseStringType.INSTANCE
				       : SimpleStringType.INSTANCE;
			}
		}
	}
}
