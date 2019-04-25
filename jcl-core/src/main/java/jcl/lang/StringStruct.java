package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.ComplexStringStructImpl;
import jcl.lang.internal.SimpleStringStructImpl;
import jcl.lang.statics.CharacterConstants;
import jcl.lang.statics.CommonLispSymbols;

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

	default StringStruct stringUpcase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return stringUpcase(context);
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

	default StringStruct stringDowncase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return stringDowncase(context);
	}

	/**
	 * Returns a new string with the contents lower-cased according to the provided {@link StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return a new string with the contents lower-cased
	 */
	StringStruct stringDowncase(final StringIntervalOpContext context);

	default StringStruct stringCapitalize(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return stringCapitalize(context);
	}

	/**
	 * Returns a new string with the contents capitalized according to the provided {@link StringIntervalOpContext}.
	 *
	 * @param context
	 * 		the interval context for the casing operation, including start and end
	 *
	 * @return a new string with the contents capitalized
	 */
	StringStruct stringCapitalize(final StringIntervalOpContext context);

	default StringStruct nStringUpcase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return nStringUpcase(context);
	}

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

	default StringStruct nStringDowncase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return nStringDowncase(context);
	}

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

	default StringStruct nStringCapitalize(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return nStringCapitalize(context);
	}

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

	default BooleanStruct stringEqual(final StringStruct string2,
	                                  final IntegerStruct start1,
	                                  final IntegerStruct end1,
	                                  final IntegerStruct start2,
	                                  final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		final boolean result = stringEqual(context);
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Determines equality of strings according to the provided {@link StringEqualityContext}. Case is accounted for.
	 *
	 * @param context
	 * 		the equality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return T if the strings are equal; NIL otherwise
	 */
	boolean stringEqual(final StringEqualityContext context);

	default LispStruct stringNotEqual(final StringStruct string2,
	                                  final IntegerStruct start1,
	                                  final IntegerStruct end1,
	                                  final IntegerStruct start2,
	                                  final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringNotEqual(context);
	}

	/**
	 * Determines inequality of strings according to the provided {@link StringEqualityContext}. Case is accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the strings are not equal; an {@link IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringNotEqual(final StringEqualityContext context);

	default LispStruct stringLessThan(final StringStruct string2,
	                                  final IntegerStruct start1,
	                                  final IntegerStruct end1,
	                                  final IntegerStruct start2,
	                                  final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringLessThan(context);
	}

	/**
	 * Determines less-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is less-than the second; an {@link IntegerStruct} mismatch index where the
	 * strings differ otherwise
	 */
	LispStruct stringLessThan(final StringEqualityContext context);

	default LispStruct stringGreaterThan(final StringStruct string2,
	                                     final IntegerStruct start1,
	                                     final IntegerStruct end1,
	                                     final IntegerStruct start2,
	                                     final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringGreaterThan(context);
	}

	/**
	 * Determines greater-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is greater-than the second; an {@link IntegerStruct} mismatch index where the
	 * strings differ otherwise
	 */
	LispStruct stringGreaterThan(final StringEqualityContext context);

	default LispStruct stringLessThanOrEqualTo(final StringStruct string2,
	                                           final IntegerStruct start1,
	                                           final IntegerStruct end1,
	                                           final IntegerStruct start2,
	                                           final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringLessThanOrEqualTo(context);
	}

	/**
	 * Determines less-than-or-equal-to inequality of strings according to the provided {@link StringEqualityContext}.
	 * Case is accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is less-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * where the strings differ otherwise
	 */
	LispStruct stringLessThanOrEqualTo(final StringEqualityContext context);

	default LispStruct stringGreaterThanOrEqualTo(final StringStruct string2,
	                                              final IntegerStruct start1,
	                                              final IntegerStruct end1,
	                                              final IntegerStruct start2,
	                                              final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringGreaterThanOrEqualTo(context);
	}

	/**
	 * Determines greater-than-or-equal-to inequality of strings according to the provided {@link
	 * StringEqualityContext}. Case is accounted for.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is greater-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * where the strings differ otherwise
	 */
	LispStruct stringGreaterThanOrEqualTo(final StringEqualityContext context);

	default BooleanStruct stringEqualIgnoreCase(final StringStruct string2,
	                                            final IntegerStruct start1,
	                                            final IntegerStruct end1,
	                                            final IntegerStruct start2,
	                                            final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		final boolean result = stringEqualIgnoreCase(context);
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Determines equality of strings according to the provided {@link StringEqualityContext}. Case is ignored.
	 *
	 * @param context
	 * 		the equality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return T if the strings are equal; NIL otherwise
	 */
	boolean stringEqualIgnoreCase(final StringEqualityContext context);

	default LispStruct stringNotEqualIgnoreCase(final StringStruct string2,
	                                            final IntegerStruct start1,
	                                            final IntegerStruct end1,
	                                            final IntegerStruct start2,
	                                            final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringNotEqualIgnoreCase(context);
	}

	/**
	 * Determines inequality of strings according to the provided {@link StringEqualityContext}. Case is ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the strings are not equal; an {@link IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringNotEqualIgnoreCase(final StringEqualityContext context);

	default LispStruct stringLessThanIgnoreCase(final StringStruct string2,
	                                            final IntegerStruct start1,
	                                            final IntegerStruct end1,
	                                            final IntegerStruct start2,
	                                            final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringLessThanIgnoreCase(context);
	}

	/**
	 * Determines less-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is less-than the second; an {@link IntegerStruct} mismatch index where the
	 * strings differ otherwise
	 */
	LispStruct stringLessThanIgnoreCase(final StringEqualityContext context);

	default LispStruct stringGreaterThanIgnoreCase(final StringStruct string2,
	                                               final IntegerStruct start1,
	                                               final IntegerStruct end1,
	                                               final IntegerStruct start2,
	                                               final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringGreaterThanIgnoreCase(context);
	}

	/**
	 * Determines greater-than inequality of strings according to the provided {@link StringEqualityContext}. Case is
	 * ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is greater-than the second; an {@link IntegerStruct} mismatch index where the
	 * strings differ otherwise
	 */
	LispStruct stringGreaterThanIgnoreCase(final StringEqualityContext context);

	default LispStruct stringLessThanOrEqualToIgnoreCase(final StringStruct string2,
	                                                     final IntegerStruct start1,
	                                                     final IntegerStruct end1,
	                                                     final IntegerStruct start2,
	                                                     final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringLessThanOrEqualToIgnoreCase(context);
	}

	/**
	 * Determines less-than-or-equal-to inequality of strings according to the provided {@link StringEqualityContext}.
	 * Case is ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is less-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * where the strings differ otherwise
	 */
	LispStruct stringLessThanOrEqualToIgnoreCase(final StringEqualityContext context);

	default LispStruct stringGreaterThanOrEqualToIgnoreCase(final StringStruct string2,
	                                                        final IntegerStruct start1,
	                                                        final IntegerStruct end1,
	                                                        final IntegerStruct start2,
	                                                        final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1)
				                     .end1(end1)
				                     .start2(start2)
				                     .end2(end2)
				                     .build();
		return stringGreaterThanOrEqualToIgnoreCase(context);
	}

	/**
	 * Determines greater-than-or-equal-to inequality of strings according to the provided {@link
	 * StringEqualityContext}. Case is ignored.
	 *
	 * @param context
	 * 		the inequality context for the equality operation, including the comparison string, start, and end
	 *
	 * @return NIL if the first string is greater-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * where the strings differ otherwise
	 */
	LispStruct stringGreaterThanOrEqualToIgnoreCase(final StringEqualityContext context);

	/**
	 * Returns whether or not the String is a 'simple' string.
	 *
	 * @return true if the String is a 'simple' string; false otherwise
	 */
	boolean isSimpleString();

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

	static StringStruct toLispString(final LispStruct lispStruct) {
		if (lispStruct instanceof StringStruct) {
			return (StringStruct) lispStruct;
		} else if (lispStruct instanceof SymbolStruct) {
			final SymbolStruct symbolStruct = (SymbolStruct) lispStruct;
			final String name = symbolStruct.getName();
			return toLispString(name);
		} else if (lispStruct instanceof CharacterStruct) {
			final CharacterStruct characterStruct = (CharacterStruct) lispStruct;
			return toLispString(characterStruct.toJavaCharacter().toString());
		} else {
			throw new TypeErrorException("Type cannot be converted to String.");
		}
	}

	static StringStruct makeString(final IntegerStruct size, final CharacterStruct initialElement,
	                               final SymbolStruct elementType) {
		final StringStruct.Builder builder = builder(size);

		builder.initialElement(initialElement);

		// TODO: elementType currently ignored!!
		final LispStruct characterType = CommonLispSymbols.CHARACTER;
		builder.elementType(characterType);

		return builder.build();
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

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof StringStruct) {
			final StringStruct string = (StringStruct) object;
			if (!length().eql(string.length())) {
				return false;
			}
			for (int i = 0; i < length().toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!char_(index).equal(string.char_(index))) {
					return false;
				}
			}
			return true;
		}
		if (object instanceof ArrayStruct) {
			// TODO: NILArray
			return object.equal(this);
		}
		return false;
	}

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof StringStruct) {
			final StringStruct string = (StringStruct) object;
			if (!length().eql(string.length())) {
				return false;
			}
			for (int i = 0; i < length().toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!char_(index).equalp(string.char_(index))) {
					return false;
				}
			}
			return true;
		}
		if (object instanceof BitVectorStruct) {
			return false;
		}
		if (object instanceof ArrayStruct) {
			return object.equalp(this);
		}
		return false;
	}

	/**
	 * Builder factory for creating {@link StringStruct} objects.
	 */
	final class Builder extends ArrayStruct.AbstractBuilder<StringStruct, LispStruct, CharacterStruct> {

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
			super(CommonLispSymbols.CHARACTER, CharacterConstants.NULL_CHAR);
			this.size = size;
		}

		@Override
		public StringStruct.Builder elementType(final LispStruct elementType) {
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
		public StringStruct.Builder adjustable(final boolean adjustable) {
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
			final int sizeInt = size.toJavaInt();
			final LispStruct upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable;
			final Integer fillPointerInt = (fillPointer == null) ? null : fillPointer.toJavaInt();

			if (displacedTo != null) {
				final LispStruct displacedToType = displacedTo.arrayElementType();
				if (!upgradedET.eq(displacedToType)) {
					throw new TypeErrorException(
							"Provided displaced to " + displacedTo + " is not an array with a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}
				// Check displaced index
				displacedTo.rowMajorAref(displacedIndexOffset);

				return new ComplexStringStructImpl(sizeInt,
				                                   upgradedET,
				                                   displacedTo,
				                                   displacedIndexOffset.toJavaInt(),
				                                   adjustableBoolean,
				                                   fillPointerInt);
			}

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					if (!element.typep(upgradedET).toJavaPBoolean()) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<CharacterStruct> validContents
						= ArrayStruct.getValidContents(Collections.singletonList(sizeInt),
						                               upgradedET,
						                               initialContents);

				final StringBuilder contents = validContents.stream()
				                                            .mapToInt(CharacterStruct::toUnicodeCodePoint)
				                                            .collect(StringBuilder::new,
				                                                     StringBuilder::appendCodePoint,
				                                                     StringBuilder::append);
				if ((fillPointerInt == null) && !adjustableBoolean) {
					return new SimpleStringStructImpl(sizeInt,
					                                  CommonLispSymbols.CHARACTER,
					                                  contents);
				}
				return new ComplexStringStructImpl(sizeInt,
				                                   CommonLispSymbols.CHARACTER,
				                                   contents,
				                                   adjustableBoolean,
				                                   fillPointerInt);
			}

			if (!initialElement.typep(upgradedET).toJavaPBoolean()) {
				// NOTE: This should never get hit due to the implementation of array-upgraded-element-type
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}

			final StringBuilder contents = Stream.generate(() -> initialElement)
			                                     .limit(sizeInt)
			                                     .mapToInt(CharacterStruct::toUnicodeCodePoint)
			                                     .collect(StringBuilder::new,
			                                              StringBuilder::appendCodePoint,
			                                              StringBuilder::append);
			if ((fillPointerInt == null) && !adjustableBoolean) {
				return new SimpleStringStructImpl(sizeInt,
				                                  CommonLispSymbols.CHARACTER,
				                                  contents);
			}
			return new ComplexStringStructImpl(sizeInt,
			                                   CommonLispSymbols.CHARACTER,
			                                   contents,
			                                   adjustableBoolean,
			                                   fillPointerInt);
		}
	}
}
