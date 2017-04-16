package jcl.lang.internal;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.function.IntPredicate;
import java.util.stream.Collectors;

import javaslang.collection.CharSeq;
import jcl.lang.AdjustArrayContext;
import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.StringEqualityContext;
import jcl.lang.StringIntervalOpContext;
import jcl.lang.StringStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.internal.stream.AbstractStringStructImpl;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.CharacterConstants;
import jcl.lang.statics.PrinterVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.StringType;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;

/**
 * The {@link StringStructImpl} is the object representation of a Lisp 'string' type.
 */
public final class StringStructImpl extends AbstractStringStructImpl implements StringStruct {

	private CharSeq charSeq;

	/**
	 * {@link StringBuilder} containing the implementation contents of the {@link StringStruct}.
	 */
	private StringBuilder contents;

	protected Integer fillPointer;
	protected boolean isAdjustable;
	protected ArrayStruct displacedTo;
	protected Integer displacedIndexOffset;

	/**
	 * Constructor for creating a new instance.
	 *
	 * @param stringType
	 * 		the {@link StringType} type of string
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link CharacterType} type of the elements
	 * @param contents
	 * 		the {@link StringBuilder} contents
	 * @param isAdjustable
	 * 		whether or not the structure is adjustable
	 * @param fillPointer
	 * 		the fill-pointer value of the structure
	 */
	public StringStructImpl(final StringType stringType, final Integer size, final CharacterType elementType,
	                        final StringBuilder contents, final boolean isAdjustable,
	                        final Integer fillPointer) {
		super(stringType, elementType, size);
		this.contents = contents;
		this.fillPointer = fillPointer;
		this.isAdjustable = isAdjustable;
	}

	/**
	 * Constructor for creating a new instance.
	 *
	 * @param stringType
	 * 		the {@link StringType} type of string
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link CharacterType} type of the elements
	 * @param displacedTo
	 * 		the {@link ArrayStruct} structure this instance will be displaced to
	 * @param displacedIndexOffset
	 * 		the offset indicating where in the displaced to structure this structures contents will start from
	 * @param isAdjustable
	 * 		whether or not the structure is adjustable
	 * @param fillPointer
	 * 		the fill-pointer value of the structure
	 */
	public StringStructImpl(final StringType stringType, final Integer size, final CharacterType elementType,
	                        final ArrayStruct displacedTo, final Integer displacedIndexOffset,
	                        final boolean isAdjustable, final Integer fillPointer) {
		super(stringType, elementType, size);
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
		this.fillPointer = fillPointer;
		this.isAdjustable = isAdjustable;
	}

	/*
	STRING-STRUCT
	 */

	@Override
	public CharacterStruct char_(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return charInternal(indexInt);
	}

	/**
	 * Internal handling of character retrieval from the underlying contents, accounting for both displaced contents and
	 * structures where the size of the structure is greater than the current number of filled contents.
	 *
	 * @param index
	 * 		the index in the contents to retrieve the {@link CharacterStruct}
	 *
	 * @return the {@link CharacterStruct} value within the contents, or {@link CharacterConstants#NULL_CHAR} if the
	 * value has yet to be populated
	 */
	private CharacterStruct charInternal(final int index) {
		if (displacedTo == null) {
			try {
				final char character = contents.charAt(index);
				return CharacterStructImpl.valueOf(character);
			} catch (final StringIndexOutOfBoundsException ignored) {
				// This is here for when the 'totalSize' is more than the contents.
				// Typically will only happen with adjusted strings.
				return CharacterConstants.NULL_CHAR;
			}
		}

		final IntegerStruct indexToGet = IntegerStructImpl.valueOf(displacedIndexOffset + index);
		return (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public CharacterStruct setfChar(final CharacterStruct newElement, final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return setfCharInternal(newElement, indexInt);
	}

	/**
	 * Internal handling of character modification within the underlying contents, accounting for displaced contents.
	 *
	 * @param newElement
	 * 		the new element to be set at the provided index location within the contents
	 * @param index
	 * 		the index in the contents to modify the existing value with the provided {@code newElement}
	 *
	 * @return newElement
	 */
	private CharacterStruct setfCharInternal(final CharacterStruct newElement, final int index) {
		if (displacedTo == null) {
			final char character = newElement.getCharacter();
			contents.setCharAt(index, character);
		} else {
			final IntegerStruct indexToSet = IntegerStructImpl.valueOf(displacedIndexOffset + index);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	@Override
	public StringStruct stringUpcase(final StringIntervalOpContext context) {
		return casifyString(context, String::toUpperCase);
	}

	@Override
	public StringStruct stringDowncase(final StringIntervalOpContext context) {
		return casifyString(context, String::toLowerCase);
	}

	@Override
	public StringStruct stringCapitalize(final StringIntervalOpContext context) {
		return casifyString(context, WordUtils::capitalize);
	}

	@Override
	public StringStruct nStringUpcase(final StringIntervalOpContext context) {
		return nCasifyString(context, String::toUpperCase);
	}

	@Override
	public StringStruct nStringDowncase(final StringIntervalOpContext context) {
		return nCasifyString(context, String::toLowerCase);
	}

	@Override
	public StringStruct nStringCapitalize(final StringIntervalOpContext context) {
		return nCasifyString(context, WordUtils::capitalize);
	}

	/**
	 * Performs the string case-altering operation function provided on the current string, utilizing the provided
	 * {@link StringIntervalOpContext} for the start and end values in determining casing boundaries.
	 *
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing the start and end values
	 * @param casifyOp
	 * 		the case-altering operation function
	 *
	 * @return a new {@link StringStruct} with case-altered contents
	 */
	private StringStruct casifyString(final StringIntervalOpContext context,
	                                  final Function<String, String> casifyOp) {
		final int startInt = getStringOpStart(context);
		final int endInt = getStringOpEnd(context, startInt);

		final String str = toJavaString(false);
		final StringBuilder builder = new StringBuilder(str);

		String strToCasify = builder.substring(startInt, endInt);
		strToCasify = casifyOp.apply(strToCasify);
		builder.replace(startInt, endInt, strToCasify);

		return new StringStructImpl((StringType) getType(),
		                            builder.length(),
		                            (CharacterType) elementType,
		                            builder,
		                            false,
		                            null);
	}

	/**
	 * Destructively modifies the current {@link #contents} or {@link #displacedTo} contents by utilizing the provided
	 * case-altering operation function and the provided {@link .StringIntervalOpContext} for the start and end values
	 * in determining casing boundaries.
	 *
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing the start and end values
	 * @param casifyOp
	 * 		the case-altering operation function
	 *
	 * @return the {@link StringStruct} instance
	 */
	private StringStruct nCasifyString(final StringIntervalOpContext context,
	                                   final Function<String, String> casifyOp) {
		final int startInt = getStringOpStart(context);
		final int endInt = getStringOpEnd(context, startInt);

		if (displacedTo == null) {
			String str = contents.substring(startInt, endInt);
			str = casifyOp.apply(str);
			contents.replace(startInt, endInt, str);
		} else {
			final StringBuilder builder = new StringBuilder();
			for (int index = startInt; index < endInt; index++) {
				final IntegerStruct indexToGet = IntegerStructImpl.valueOf(displacedIndexOffset + index);
				final CharacterStruct character = (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
				builder.appendCodePoint(character.getCodePoint());
			}

			String str = builder.toString();
			str = casifyOp.apply(str);

			for (int updateIndex = startInt, stringIndex = 0;
			     updateIndex < endInt;
			     updateIndex++, stringIndex++) {
				final IntegerStruct indexToSet = IntegerStructImpl.valueOf(displacedIndexOffset + updateIndex);
				final char c = str.charAt(stringIndex);
				final CharacterStruct character = CharacterStructImpl.valueOf(c);
				displacedTo.setfRowMajorAref(character, indexToSet);
			}
		}
		return this;
	}

	@Override
	public StringStruct stringTrim(final SequenceStruct characterBag) {
		return trimString(characterBag, StringUtils::strip);
	}

	@Override
	public StringStruct stringLeftTrim(final SequenceStruct characterBag) {
		return trimString(characterBag, StringUtils::stripStart);
	}

	@Override
	public StringStruct stringRightTrim(final SequenceStruct characterBag) {
		return trimString(characterBag, StringUtils::stripEnd);
	}

	/**
	 * Trims the string based on the provided trimming operation and the {@link SequenceStruct} character bag.
	 *
	 * @param characterBag
	 * 		the collection of characters to trim from the string
	 * @param trimOp
	 * 		the trimming operation to perform
	 *
	 * @return a new string with the bag of characters trimmed from the string
	 */
	private StringStruct trimString(final SequenceStruct characterBag,
	                                final BiFunction<String, String, String> trimOp) {
		final List<LispStruct> nonCharacters
				= characterBag.stream()
				              .filter(element -> !(element instanceof CharacterStruct))
				              .collect(Collectors.toList());
		if (!nonCharacters.isEmpty()) {
			throw new TypeErrorException("Non-character elements provided in character bag: " + nonCharacters);
		}

		final String stripChars
				= characterBag.stream()
				              .map(CharacterStruct.class::cast)
				              .map(CharacterStruct::getCodePoint)
				              .collect(StringBuilder::new,
				                       StringBuilder::appendCodePoint,
				                       StringBuilder::append)
				              .toString();

		final String str = toJavaString(false);
		final String trimmedString = trimOp.apply(str, stripChars);
		return new StringStructImpl((StringType) getType(),
		                            trimmedString.length(),
		                            (CharacterType) elementType,
		                            new StringBuilder(trimmedString),
		                            false,
		                            null);
	}

	@Override
	public BooleanStruct stringEqual(final StringEqualityContext context) {
		return equalComparison(context,
		                       String::compareTo);
	}

	@Override
	public LispStruct stringNotEqual(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x != 0,
		                            StringStructImpl::indexOfSameness);
	}

	@Override
	public LispStruct stringLessThan(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x < 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThan(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x > 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringLessThanOrEqualTo(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x <= 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThanOrEqualTo(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x >= 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public BooleanStruct stringEqualIgnoreCase(final StringEqualityContext context) {
		return equalComparison(context,
		                       String::compareToIgnoreCase);
	}

	@Override
	public LispStruct stringNotEqualIgnoreCase(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x != 0,
		                            StringStructImpl::indexOfSameness);
	}

	@Override
	public LispStruct stringLessThanIgnoreCase(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x < 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThanIgnoreCase(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x > 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringLessThanOrEqualToIgnoreCase(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x <= 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThanOrEqualToIgnoreCase(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x >= 0,
		                            StringUtils::indexOfDifference);
	}

	/**
	 * Compares this string using the provided comparison function with the {@link StringEqualityContext#struct}
	 * string, utilizing the provided {@link StringEqualityContext} for the start and end values for each string in the
	 * comparison.
	 *
	 * @param context
	 * 		the {@link StringEqualityContext} containing the string to compare to as well as start and end values for each
	 * 		string in the comparison
	 * @param stringCompareToOp
	 * 		the equality comparison function
	 *
	 * @return true if the strings are equal; false otherwise
	 */
	private BooleanStruct equalComparison(final StringEqualityContext context,
	                                      final BiFunction<String, String, Integer> stringCompareToOp) {
		final EqualityStrings equalityStrings = getEqualityStrings(context);
		final String str1 = equalityStrings.str1;
		final String str2 = equalityStrings.str2;

		final int result = stringCompareToOp.apply(str1, str2);
		return LispStructFactory.toBoolean(result == 0);
	}

	/**
	 * Compares this string using the provided comparison function with the {@link StringEqualityContext#struct}
	 * string, utilizing the provided {@link StringEqualityContext} for the start and end values for each string in the
	 * comparison.
	 *
	 * @param context
	 * 		the {@link StringEqualityContext} containing the string to compare to as well as start and end values for each
	 * 		string in the comparison
	 * @param stringCompareToOp
	 * 		the equality comparison function
	 * @param comparisonOp
	 * 		the numeric comparison operation for determining level of equivalence
	 * @param mismatchIndexOp
	 * 		the operation for locating the mismatched character index in the case of inequality
	 *
	 * @return {@link NILStruct#INSTANCE} if the strings are equal; the mismatching index otherwise
	 */
	private LispStruct inequalityComparison(final StringEqualityContext context,
	                                        final BiFunction<String, String, Integer> stringCompareToOp,
	                                        final IntPredicate comparisonOp,
	                                        final BiFunction<String, String, Integer> mismatchIndexOp) {
		final EqualityStrings equalityStrings = getEqualityStrings(context);
		final String str1 = equalityStrings.str1;
		final String str2 = equalityStrings.str2;

		final int result = stringCompareToOp.apply(str1, str2);
		if (comparisonOp.test(result)) {
			final int mismatchIndex = mismatchIndexOp.apply(str1, str2);
			return IntegerStructImpl.valueOf(mismatchIndex);
		} else {
			return NILStruct.INSTANCE;
		}
	}

	/**
	 * Adapted from {@link StringUtils#indexOfDifference(CharSequence, CharSequence)} to determine the index where the
	 * two provided {@link CharSequence} objects are the same.
	 *
	 * @param cs1
	 * 		the first {@link CharSequence}
	 * @param cs2
	 * 		the second {@link CharSequence}
	 *
	 * @return the index where cs1 and cs2 begin to differ; -1 if they are equal
	 */
	private static int indexOfSameness(final CharSequence cs1, final CharSequence cs2) {
		if (cs1 == cs2) {
			return 0;
		}
		if ((cs1 == null) || (cs2 == null)) {
			return StringUtils.INDEX_NOT_FOUND;
		}
		int i;
		for (i = 0; (i < cs1.length()) && (i < cs2.length()); ++i) {
			if (cs1.charAt(i) == cs2.charAt(i)) {
				break;
			}
		}
		if ((i < cs2.length()) || (i < cs1.length())) {
			return i;
		}
		return StringUtils.INDEX_NOT_FOUND;
	}

	/**
	 * Retrieves an {@link EqualityStrings} object containing the two strings to compare for equality, accounting for
	 * starting and ending indexes for both strings.
	 *
	 * @param context
	 * 		the {@link StringEqualityContext} containing the string to compare this string to as well as starting and
	 * 		ending indicies for each string in the equality operation
	 *
	 * @return an {@link EqualityStrings} object containing the two strings to compare for equality
	 */
	private EqualityStrings getEqualityStrings(final StringEqualityContext context) {
		final StringIntervalOpContext context1 = context.getContext1();
		final StringIntervalOpContext context2 = context.getContext2();

		final int start1 = getStringOpStart(context1);
		final int end1 = getStringOpEnd(context1, start1);

		final int start2 = getStringOpStart(context2);
		final int end2 = getStringOpEnd(context2, start2);

		final String str1 = toJavaString(false);
		final String str2 = context.getStruct()
		                           .toJavaString(false);

		final String subStr1 = str1.substring(start1, end1);
		final String subStr2 = str2.substring(start2, end2);
		return new EqualityStrings(subStr1, subStr2);
	}

	/**
	 * A wrapper for the {@link String} objects to be used in an equality operation.
	 */
	private static final class EqualityStrings {

		/**
		 * The first string to compare.
		 */
		final String str1;

		/**
		 * The second string to compare.
		 */
		final String str2;

		/**
		 * Private constructor initializing the object.
		 *
		 * @param str1
		 * 		the first string to compare
		 * @param str2
		 * 		the second string to compare
		 */
		private EqualityStrings(final String str1, final String str2) {
			this.str1 = str1;
			this.str2 = str2;
		}
	}

	@Override
	public String toJavaString(final boolean ignoreFillPointer) {
		if (displacedTo != null) {
			return getDisplacedToAsJavaString(ignoreFillPointer);
		}
		if (!ignoreFillPointer && (fillPointer != null)) {
			return contents.substring(0, fillPointer);
		}
		return contents.toString();
	}

	/**
	 * Returns this displaced string value as a {@link String}, ignoring the fill-pointer value according to the
	 * provided {@code ignoreFillPointer} value.
	 *
	 * @param ignoreFillPointer
	 * 		whether or not to ignore the fill-pointer value of the string
	 *
	 * @return a {@link String} representation of the displaced string
	 */
	private String getDisplacedToAsJavaString(final boolean ignoreFillPointer) {
		final int size = (!ignoreFillPointer && (fillPointer != null)) ? fillPointer : totalSize;
		final StringBuilder builder = new StringBuilder();
		for (int index = 0; index < size; index++) {
			final IntegerStruct indexToGet = IntegerStructImpl.valueOf(displacedIndexOffset + index);
			final CharacterStruct character = (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
			builder.appendCodePoint(character.getCodePoint());
		}
		return builder.toString();
	}

	/**
	 * Retrieves the starting value from the {@link StringIntervalOpContext} for a string operation.
	 *
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing starting and ending values for string operations
	 *
	 * @return the start value for the string operation
	 */
	private int getStringOpStart(final StringIntervalOpContext context) {
		final IntegerStruct start = context.getStart();
		final int startInt;
		if (start == null) {
			startInt = 0;
		} else {
			startInt = start.intValue();
			final int observedLength = (fillPointer == null) ? totalSize : fillPointer;
			if ((startInt < 0) || (startInt > observedLength)) {
				throw new ErrorException(
						"Bad start value " + start + " for string with size: " + observedLength);
			}
		}
		return startInt;
	}

	/**
	 * Retrieves the ending value from the {@link StringIntervalOpContext} for a string operation.
	 *
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing starting and ending values for string operations
	 * @param startInt
	 * 		the starting value for the string operation
	 *
	 * @return the end value for the string operation
	 */
	private int getStringOpEnd(final StringIntervalOpContext context, final int startInt) {
		final IntegerStruct end = context.getEnd();
		final int endInt;
		if (end == null) {
			endInt = (fillPointer == null) ? totalSize : fillPointer;
		} else {
			endInt = end.intValue();
			final int observedLength = (fillPointer == null) ? totalSize : fillPointer;
			if ((endInt < 0) || (endInt > observedLength) || (endInt < startInt)) {
				throw new ErrorException(
						"Bad end value " + end + " with start value " + startInt + " for string with size: " + observedLength);
			}
		}
		return endInt;
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public IntegerStruct fillPointer() {
		if (fillPointer == null) {
			throw new TypeErrorException("STRING has no fill-pointer to retrieve.");
		}
		return IntegerStructImpl.valueOf(fillPointer);
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		final int intValue = fillPointer.intValue();
		if ((intValue < 0) || (intValue > totalSize)) {
			throw new ErrorException(
					"Fill-pointer " + fillPointer + " value is out of bounds for STRING with size " + totalSize + '.');
		}

		this.fillPointer = intValue;
		return fillPointer;
	}

	@Override
	public CharacterStruct vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a STRING with no fill-pointer.");
		}
		if (fillPointer == 0) {
			throw new ErrorException("Nothing left to pop.");
		}

		return charInternal(--fillPointer);
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a STRING with no fill-pointer.");
		}
		if (fillPointer >= totalSize) {
			return NILStruct.INSTANCE;
		}

		setfCharInternal((CharacterStruct) newElement, fillPointer++);
		return IntegerStructImpl.valueOf(fillPointer);
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement,
	                                      final IntegerStruct extension) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a STRING with no fill-pointer.");
		}
		if (fillPointer >= totalSize) {
			if (!isAdjustable) {
				throw new TypeErrorException("VECTOR would be extended and is not adjustable.");
			}
			if (displacedTo == null) {
				final int currentContentSize = contents.length();
				contents.ensureCapacity(currentContentSize + extension.intValue());
				contents.setLength(currentContentSize + 1);
			} else {
				final String displacedContents = getDisplacedToAsJavaString(false);
				contents = new StringBuilder(displacedContents.length() + extension.intValue());
				contents.append(displacedContents);
				contents.setLength(contents.length() + 1);

				displacedTo = null;
				displacedIndexOffset = null;
			}
		}

		setfCharInternal((CharacterStruct) newElement, fillPointer++);
		return IntegerStructImpl.valueOf(fillPointer);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final LispStruct initialElement, final IntegerStruct fillPointer) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final SequenceStruct initialContents, final IntegerStruct fillPointer) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final IntegerStruct fillPointer, final ArrayStruct displacedTo,
	                               final IntegerStruct displacedIndexOffset) {
		throw new UnsupportedOperationException();
	}

	@Override
	public StringStruct adjustArray(final AdjustArrayContext context) {

		final List<IntegerStruct> newDimensions = context.getDimensions();
		final LispType newElementType = context.getElementType();
		final SequenceStruct newInitialContents = context.getInitialContents();
		final ArrayStruct newDisplacedTo = context.getDisplacedTo();

		if (newDimensions.size() != 1) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}
		final LispType upgradedET = (newElementType == null)
		                            ? elementType
		                            : ArrayStruct.upgradedArrayElementType(newElementType);

		if (!elementType.equals(upgradedET)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + elementType + '.');
		}

		if (newDisplacedTo != null) {
			return adjustDisplacedTo(context, upgradedET);
		} else if (newInitialContents != null) {
			return adjustInitialContents(context, upgradedET);
		} else {
			return adjustInitialElement(context, upgradedET);
		}
	}

	/**
	 * Performs adjust-array functionality when attempting to adjust to a displaced array. If the original array was
	 * adjustable, the innards will be adjusted so that the contents are no longer valid and the {@link #displacedTo}
	 * contains the content value. If the original array was not adjustable, the information between the original array
	 * and the {@link AdjustArrayContext} adjusting parameters will produce a new array adjusted accordingly.
	 *
	 * @param context
	 * 		the {@link AdjustArrayContext} containing the adjusting parameters
	 * @param upgradedET
	 * 		the element-type that represents the upgraded array-element-type for the resulting adjustment
	 *
	 * @return either the current instance, if the original array was adjustable, or a new instance adjusted accordingly
	 */
	private StringStruct adjustDisplacedTo(final AdjustArrayContext context, final LispType upgradedET) {

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final BooleanStruct newAdjustable = context.getAdjustable();
		final IntegerStruct newFillPointer = context.getFillPointer();
		final ArrayStruct newDisplacedTo = context.getDisplacedTo();
		final IntegerStruct newDisplacedIndexOffset = context.getDisplacedIndexOffset();

		final LispType displacedElementType = newDisplacedTo.arrayElementType();
		if (!upgradedET.equals(displacedElementType)) {
			throw new TypeErrorException(
					"Provided array for displacement " + newDisplacedTo + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		try {
			newDisplacedTo.rowMajorAref(newDisplacedIndexOffset);
		} catch (final ErrorException ignored) {
			throw new ErrorException("Requested size is too large to displace to " + newDisplacedTo + '.');
		}

		if (isAdjustable) {
			totalSize = newTotalSize.intValue();
			elementType = upgradedET;
			isAdjustable = newAdjustable.booleanValue();
			fillPointer = (newFillPointer == null) ? null : newFillPointer.intValue();
			contents = null;
			displacedTo = newDisplacedTo;
			displacedIndexOffset = newDisplacedIndexOffset.intValue();
			return this;
		} else {
			return StringStruct.builder(newTotalSize)
			                   .elementType((CharacterType) upgradedET)
			                   .adjustable(newAdjustable)
			                   .fillPointer(newFillPointer)
			                   .displacedTo(newDisplacedTo)
			                   .displacedIndexOffset(newDisplacedIndexOffset)
			                   .build();
		}
	}

	/**
	 * Performs adjust-array functionality when attempting to adjust with a new sequence of contents. If the original
	 * array was adjustable, the innards will be adjusted so that any displacements are no longer valid and the contents
	 * contain the content value. If the original array was not adjustable, the information between the original array
	 * and the {@link AdjustArrayContext} adjusting parameters will produce a new array adjusted accordingly.
	 *
	 * @param context
	 * 		the {@link AdjustArrayContext} containing the adjusting parameters
	 * @param upgradedET
	 * 		the element-type that represents the upgraded array-element-type for the resulting adjustment
	 *
	 * @return either the current instance, if the original array was adjustable, or a new instance adjusted accordingly
	 */
	private StringStruct adjustInitialContents(final AdjustArrayContext context, final LispType upgradedET) {

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final SequenceStruct newInitialContents = context.getInitialContents();
		final BooleanStruct newAdjustable = context.getAdjustable();
		final IntegerStruct newFillPointer = context.getFillPointer();

		for (final LispStruct initialElement : newInitialContents) {
			final LispType currentElementType = initialElement.getType();
			if (currentElementType.isNotOfType(upgradedET)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		if (isAdjustable) {
			final int newTotalSizeInt = newTotalSize.intValue();
			final boolean newAdjustableBoolean = newAdjustable.booleanValue();
			final Integer newFillPointerInt = (newFillPointer == null) ? null : newFillPointer.intValue();

			final List<CharacterStruct> validContents
					= ArrayStruct.getValidContents(Collections.singletonList(newTotalSizeInt),
					                               upgradedET,
					                               newInitialContents);
			contents = validContents.stream()
			                        .mapToInt(CharacterStruct::getCodePoint)
			                        .collect(StringBuilder::new,
			                                 StringBuilder::appendCodePoint,
			                                 StringBuilder::append);

			totalSize = newTotalSizeInt;
			elementType = upgradedET;
			isAdjustable = newAdjustableBoolean;
			fillPointer = newFillPointerInt;
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return StringStruct.builder(newTotalSize)
			                   .elementType((CharacterType) upgradedET)
			                   .adjustable(newAdjustable)
			                   .fillPointer(newFillPointer)
			                   .initialContents(newInitialContents)
			                   .build();
		}
	}

	/**
	 * Performs adjust-array functionality when attempting to adjust with a new provided element value. If the original
	 * array was adjustable, the innards will be adjusted so that any displacements are no longer valid and the
	 * contents contain filled in value with the provided element. If the original array was not adjustable, the
	 * information between the original array and the {@link AdjustArrayContext} adjusting parameters will produce a new
	 * array adjusted accordingly.
	 *
	 * @param context
	 * 		the {@link AdjustArrayContext} containing the adjusting parameters
	 * @param upgradedET
	 * 		the element-type that represents the upgraded array-element-type for the resulting adjustment
	 *
	 * @return either the current instance, if the original array was adjustable, or a new instance adjusted accordingly
	 */
	private StringStruct adjustInitialElement(final AdjustArrayContext context, final LispType upgradedET) {
		final LispStruct newInitialElement = context.getInitialElement();

		if (newInitialElement != null) {
			if (!(newInitialElement instanceof CharacterStruct)) {
				throw new TypeErrorException(
						"Provided element " + newInitialElement + " is not a CHARACTER.");
			}

			final LispType initialElementType = newInitialElement.getType();
			if (!upgradedET.equals(initialElementType)) {
				throw new TypeErrorException(
						"Provided element " + newInitialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final int newTotalSizeInt = newTotalSize.intValue();

		final BooleanStruct newAdjustable = context.getAdjustable();
		final boolean newAdjustableBoolean = newAdjustable.booleanValue();

		final IntegerStruct newFillPointer = context.getFillPointer();
		final Integer newFillPointerInt = (newFillPointer == null) ? null : newFillPointer.intValue();

		if (isAdjustable) {
			if (displacedTo == null) {
				updateContentsWithElement(contents, newInitialElement, totalSize, newTotalSizeInt);
			} else {
				final String displacedContents = getDisplacedToAsJavaString(true);
				contents = new StringBuilder(displacedContents);
			}
			updateContentsWithElement(contents, newInitialElement, totalSize, newTotalSizeInt);

			totalSize = newTotalSizeInt;
			elementType = upgradedET;
			isAdjustable = newAdjustableBoolean;
			fillPointer = newFillPointerInt;
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			final StringBuilder newContents;
			if (displacedTo == null) {
				newContents = new StringBuilder(contents);
			} else {
				final String displacedContents = getDisplacedToAsJavaString(true);
				newContents = new StringBuilder(displacedContents);
			}
			updateContentsWithElement(newContents, newInitialElement, totalSize, newTotalSizeInt);

			final StringType stringType = StringStruct.Builder.getStringType(
					newAdjustableBoolean,
					newFillPointerInt,
					upgradedET
			);
			return new StringStructImpl(stringType,
			                            newTotalSizeInt,
			                            (CharacterType) upgradedET,
			                            newContents,
			                            newAdjustableBoolean,
			                            newFillPointerInt);
		}
	}

	/**
	 * A special case for adjusting by initial-element, where the size is altered. If the new size is less than the old
	 * size, the contents are reduce and the new element is not added. However, if the new size is greater than the old
	 * size, the new content slots will be filled with the new element.
	 *
	 * @param newContents
	 * 		the new contents container
	 * @param newElement
	 * 		the new element to be used when filling in the new contents
	 * @param oldTotalSize
	 * 		the size of the original array
	 * @param newTotalSizeInt
	 * 		the size the array is to be adjusted to
	 */
	private static void updateContentsWithElement(final StringBuilder newContents, final LispStruct newElement,
	                                              final int oldTotalSize, final int newTotalSizeInt) {
		if (newTotalSizeInt < oldTotalSize) {
			newContents.delete(newTotalSizeInt, newContents.length());
		} else if (newTotalSizeInt > oldTotalSize) {
			newContents.ensureCapacity(newTotalSizeInt);
			for (int i = oldTotalSize; i < newTotalSizeInt; i++) {
				if (newElement != null) {
					final int codePoint = ((CharacterStruct) newElement).getCodePoint();
					newContents.appendCodePoint(codePoint);
				}
			}
		}
	}

	@Override
	public BooleanStruct adjustableArrayP() {
		return LispStructFactory.toBoolean(isAdjustable);
	}

	@Override
	public CharacterStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		final int rowMajorIndex = validateSubscript(subscript);
		return charInternal(rowMajorIndex);
	}

	@Override
	public CharacterStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}

		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		final int rowMajorIndex = validateSubscript(subscript);
		return setfCharInternal((CharacterStruct) newElement, rowMajorIndex);
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return LispStructFactory.toBoolean(fillPointer != null);
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStructImpl.valueOf(displacedIndexOffset));
	}

	@Override
	public CharacterStruct rowMajorAref(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return charInternal(indexInt);
	}

	@Override
	public CharacterStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}

		final int indexInt = validateSubscript(index);
		return setfCharInternal((CharacterStruct) newElement, indexInt);
	}

// =================

	@Override
	public List<LispStruct> getContents() {
		return null;
	}

// =================

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public IntegerStruct length() {
		if (fillPointer != null) {
			return IntegerStructImpl.valueOf(fillPointer);
		}
		return IntegerStructImpl.valueOf(totalSize);
	}

	@Override
	public CharacterStruct elt(final IntegerStruct index) {
		final int indexInt = validateIndexAgainstFillPointer(index);
		return charInternal(indexInt);
	}

	@Override
	public CharacterStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}

		final int indexInt = validateIndexAgainstFillPointer(index);
		return setfCharInternal((CharacterStruct) newElement, indexInt);
	}

	private int validateIndexAgainstFillPointer(final IntegerStruct index) {
		if (fillPointer != null) {
			final int indexInt = index.intValue();
			if (indexInt > fillPointer) {
				throw new ErrorException(index + " is not a valid sequence index for " + this);
			}
		}
		return validateSubscript(index);
	}

	@Override
	public StringStruct reverse() {
		final StringBuilder reversedContents;
		if (displacedTo != null) {
			final String contentsToReverse = getDisplacedToAsJavaString(false);
			reversedContents = new StringBuilder(contentsToReverse).reverse();
		} else if (fillPointer == null) {
			final String contentsToReverse = contents.toString();
			reversedContents = new StringBuilder(contentsToReverse).reverse();
		} else {
			final String contentsToReverse = contents.substring(0, fillPointer);
			reversedContents = new StringBuilder(contentsToReverse).reverse();
		}
		return new StringStructImpl((StringType) getType(),
		                            totalSize,
		                            (CharacterType) elementType,
		                            reversedContents,
		                            isAdjustable,
		                            fillPointer);
	}

	@Override
	public StringStruct nReverse() {
		if (displacedTo != null) {
			final String contentsToReverse = getDisplacedToAsJavaString(false);
			final StringBuilder reversedContent = new StringBuilder(contentsToReverse).reverse();

			for (int index = 0; index < reversedContent.length(); index++) {
				final IntegerStruct indexToSet = IntegerStructImpl.valueOf(displacedIndexOffset + index);
				final char c = reversedContent.charAt(index);
				final CharacterStruct character = CharacterStructImpl.valueOf(c);
				displacedTo.setfRowMajorAref(character, indexToSet);
			}
		} else if (fillPointer == null) {
			contents.reverse();
		} else {
			final String contentsToReverse = contents.substring(0, fillPointer);
			final String reversedContent = new StringBuilder(contentsToReverse).reverse()
			                                                                   .toString();
			contents.replace(0, fillPointer, reversedContent);
		}
		return this;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		if (displacedTo == null) {
			return new StringStructImpl.StringIterator(contents);
		} else {
			return new StringStructImpl.DisplacedStringIterator(totalSize, displacedTo, displacedIndexOffset);
		}
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(iterator(),
		                                totalSize,
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.IMMUTABLE |
				                                Spliterator.SUBSIZED
		);
	}

	/**
	 * Iterator for {@link StringStruct} structures without displaced contents.
	 */
	private static final class StringIterator implements Iterator<LispStruct> {

		/**
		 * The contents of the {@link StringStructImpl} being iterated over.
		 */
		private final StringBuilder contents;

		/**
		 * The current index of the iteration.
		 */
		private int current;

		/**
		 * Constructor for building the iterator.
		 *
		 * @param contents
		 * 		the contents of the {@link StringStructImpl} to be iterated over
		 */
		private StringIterator(final StringBuilder contents) {
			this.contents = contents;
		}

		@Override
		public boolean hasNext() {
			try {
				contents.charAt(current);
				return true;
			} catch (final StringIndexOutOfBoundsException ignored) {
				return false;
			}
		}

		@Override
		public LispStruct next() {
			final char character;
			try {
				character = contents.charAt(current);
			} catch (final StringIndexOutOfBoundsException ignored) {
				throw new NoSuchElementException("All elements consumed.");
			} finally {
				current++;
			}
			return CharacterStructImpl.valueOf(character);
		}
	}

	/**
	 * Iterator for {@link StringStruct} structures with displaced contents.
	 */
	private static final class DisplacedStringIterator implements Iterator<LispStruct> {

		/**
		 * The total size of the contents of the {@link StringStructImpl} structure.
		 */
		private final int totalSize;

		/**
		 * The {@link ArrayStruct} the {@link StringStructImpl} is displaced to.
		 */
		private final ArrayStruct displacedTo;

		/**
		 * The offset value into the displaced array structure where the {@link StringStructImpl} starts.
		 */
		private final int displacedIndexOffset;

		/**
		 * The current index of the iteration.
		 */
		private int current;

		/**
		 * Constructor for building the iterator.
		 *
		 * @param totalSize
		 * 		the total size of the contents of the {@link StringStructImpl} structure
		 * @param displacedTo
		 * 		the {@link ArrayStruct} the {@link StringStructImpl} is displaced to
		 * @param displacedIndexOffset
		 * 		the offset value into the displaced array structure where the {@link StringStructImpl} starts
		 */
		private DisplacedStringIterator(final int totalSize,
		                                final ArrayStruct displacedTo,
		                                final int displacedIndexOffset) {
			this.totalSize = totalSize;
			this.displacedTo = displacedTo;
			this.displacedIndexOffset = displacedIndexOffset;
		}

		@Override
		public boolean hasNext() {
			return current < totalSize;
		}

		@Override
		public LispStruct next() {
			if (current < totalSize) {
				final IntegerStruct indexToGet = IntegerStructImpl.valueOf(displacedIndexOffset + current);
				current++;
				return displacedTo.rowMajorAref(indexToGet);
			}
			throw new NoSuchElementException("All elements consumed.");
		}
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue().booleanValue();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append('"');
		}

		final IntConsumer appendFn = codePoint -> {
			final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);
			if ((codePoint == '"') || (syntaxType == SyntaxType.SINGLE_ESCAPE)) {
				stringBuilder.append('\\');
			}
			stringBuilder.appendCodePoint(codePoint);
		};

		if (displacedTo != null) {
			final String str = getDisplacedToAsJavaString(false);
			str.codePoints()
			   .forEach(appendFn);
		} else if (fillPointer == null) {
			contents.codePoints()
			        .forEach(appendFn);
		} else {
			contents.codePoints()
			        .limit(fillPointer)
			        .forEach(appendFn);
		}

		if (printEscape) {
			stringBuilder.append('"');
		}

		return stringBuilder.toString();
	}
}
