package jcl.lang.internal;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntPredicate;
import java.util.stream.Collectors;

import jcl.lang.AdjustArrayContext;
import jcl.lang.ArrayStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.StringEqualityContext;
import jcl.lang.StringIntervalOpContext;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CharacterConstants;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.StringType;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

/**
 * Base class for all {@link StringStruct} implementation structures.
 */
public abstract class AbstractStringStructImpl extends AbstractVectorStructImpl implements StringStruct {

	/**
	 * Protected constructor for initializing {@link #type}, {@link #elementType}, and {@link #totalSize} properties.
	 *
	 * @param type
	 * 		the value used to initialize {@link #type} property
	 * @param elementType
	 * 		the value used to initialize {@link #elementType} property
	 * @param totalSize
	 * 		the value used to initialize {@link #totalSize} property
	 */
	protected AbstractStringStructImpl(final StringType type, final CharacterType elementType,
	                                   final Integer totalSize) {
		super(type, elementType, totalSize);
	}

	// =================
	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final LispStruct initialElement, final IntegerStruct fillPointer) {
		// TODO: Remove
		throw new UnsupportedOperationException();
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final SequenceStruct initialContents, final IntegerStruct fillPointer) {
		// TODO: Remove
		throw new UnsupportedOperationException();
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final IntegerStruct fillPointer, final ArrayStruct displacedTo,
	                               final IntegerStruct displacedIndexOffset) {
		// TODO: Remove
		throw new UnsupportedOperationException();
	}

	@Override
	public List<LispStruct> getContents() {
		// TODO: Remove
		return null;
	}
// =================

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
	protected abstract CharacterStruct charInternal(final int index);

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
	protected abstract CharacterStruct setfCharInternal(final CharacterStruct newElement, final int index);

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

		return new SimpleStringStructImpl(builder.length(), (CharacterType) elementType, builder);
	}

	/**
	 * Destructively modifies the current contents or possible displaced contents by utilizing the provided
	 * case-altering operation function and the provided {@link StringIntervalOpContext} for the start and end values
	 * in determining casing boundaries.
	 *
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing the start and end values
	 * @param casifyOp
	 * 		the case-altering operation function
	 *
	 * @return the {@link StringStruct} instance
	 */
	protected abstract StringStruct nCasifyString(final StringIntervalOpContext context,
	                                              final Function<String, String> casifyOp);

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
				              .map(CharacterStruct::toUnicodeCodePoint)
				              .collect(StringBuilder::new,
				                       StringBuilder::appendCodePoint,
				                       StringBuilder::append)
				              .toString();

		final String str = toJavaString(false);
		final String trimmedString = trimOp.apply(str, stripChars);
		return new SimpleStringStructImpl(trimmedString.length(),
		                                  (CharacterType) elementType,
		                                  new StringBuilder(trimmedString));
	}

	@Override
	public boolean stringEqual(final StringEqualityContext context) {
		return equalComparison(context,
		                       String::compareTo);
	}

	@Override
	public LispStruct stringNotEqual(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x != 0,
		                            StringUtils::indexOfDifference);
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
	public boolean stringEqualIgnoreCase(final StringEqualityContext context) {
		return equalComparison(context,
		                       String::compareToIgnoreCase);
	}

	@Override
	public LispStruct stringNotEqualIgnoreCase(final StringEqualityContext context) {
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x != 0,
		                            StringUtils::indexOfDifference);
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
	private boolean equalComparison(final StringEqualityContext context,
	                                      final BiFunction<String, String, Integer> stringCompareToOp) {
		final EqualityStrings equalityStrings = getEqualityStrings(context);
		final String str1 = equalityStrings.str1;
		final String str2 = equalityStrings.str2;

		final int result = stringCompareToOp.apply(str1, str2);
		return result == 0;
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
			if (mismatchIndex == -1) {
				return IntegerStruct.toLispInteger(Math.min(str1.length(), str2.length()));
			}
			return IntegerStruct.toLispInteger(mismatchIndex);
		} else {
			return NILStruct.INSTANCE;
		}
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

	/**
	 * Retrieves the starting value from the {@link StringIntervalOpContext} for a string operation.
	 *
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing starting and ending values for string operations
	 *
	 * @return the start value for the string operation
	 */
	protected int getStringOpStart(final StringIntervalOpContext context) {
		final IntegerStruct start = context.getStart();
		final int startInt;
		if (start == null) {
			startInt = 0;
		} else {
			startInt = start.toJavaInt();
			final int observedLength = getObservedLength();
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
	protected int getStringOpEnd(final StringIntervalOpContext context, final int startInt) {
		final IntegerStruct end = context.getEnd();
		final int endInt;
		if (end == null) {
			endInt = getObservedLength();
		} else {
			endInt = end.toJavaInt();
			final int observedLength = getObservedLength();
			if ((endInt < 0) || (endInt > observedLength) || (endInt < startInt)) {
				throw new ErrorException(
						"Bad end value " + end + " with start value " + startInt + " for string with size: " + observedLength);
			}
		}
		return endInt;
	}

	/**
	 * Helper method for {@link #getStringOpStart(StringIntervalOpContext)} and {@link
	 * #getStringOpEnd(StringIntervalOpContext, int)} methods for getting observed length values. This is used for
	 * dealing with things like fill-pointers in complex structures.
	 *
	 * @return the observed length value for interval operations
	 */
	protected int getObservedLength() {
		return totalSize;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public StringStruct adjustArray(final AdjustArrayContext context) {

		final List<IntegerStruct> newDimensions = context.getDimensions();
		final LispType newElementType = context.getElementType();
		final SequenceStruct newInitialContents = context.getInitialContents();
		final ArrayStruct newDisplacedTo = context.getDisplacedTo();

		if (newDimensions.size() != 1) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}

		LispType upgradedET = elementType;
		if (newElementType != null) {
			if (!elementType.typeEquals(newElementType)) {
				throw new TypeErrorException(
						"Provided element-type " + newElementType + " must be a subtype of the initial upgraded-array-element-type " + elementType + '.');
			}
			upgradedET = ArrayStruct.upgradedArrayElementType(newElementType);
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
	 * adjustable, the innards will be adjusted so that the contents are no longer valid and the displaced contents
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
	protected abstract StringStruct adjustDisplacedTo(final AdjustArrayContext context, final LispType upgradedET);

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
	protected abstract StringStruct adjustInitialContents(final AdjustArrayContext context, final LispType upgradedET);

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
	protected abstract StringStruct adjustInitialElement(final AdjustArrayContext context, final LispType upgradedET);

	/**
	 * Validates that the newly provided initial-element value for array adjustment is a subtype of the provided
	 * upgraded-array-element-type.
	 *
	 * @param newInitialElement
	 * 		the new initial-element
	 * @param upgradedET
	 * 		the upgraded-array-element-type
	 */
	protected static void validateNewInitialElement(final LispStruct newInitialElement, final LispType upgradedET) {
		if (newInitialElement != null) {
			if (!(newInitialElement instanceof CharacterStruct)) {
				throw new TypeErrorException(
						"Provided element " + newInitialElement + " is not a CHARACTER.");
			}

			// NOTE: Should never hit this in reality, but keeping this check here.
			final LispType initialElementType = newInitialElement.getType();
			if (!upgradedET.typeEquals(initialElementType)) {
				throw new TypeErrorException(
						"Provided element " + newInitialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
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
	protected static void updateContentsWithElement(final StringBuilder newContents, final LispStruct newElement,
	                                                final int oldTotalSize, final int newTotalSizeInt) {
		if (newTotalSizeInt < oldTotalSize) {
			newContents.delete(newTotalSizeInt, newContents.length());
		} else if (newTotalSizeInt > oldTotalSize) {
			newContents.ensureCapacity(newTotalSizeInt);
			for (int i = oldTotalSize; i < newTotalSizeInt; i++) {
				if (newElement != null) {
					final int codePoint = ((CharacterStruct) newElement).toUnicodeCodePoint();
					newContents.appendCodePoint(codePoint);
				}
			}
		}
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

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public IntegerStruct length() {
		return IntegerStruct.toLispInteger(totalSize);
	}

	@Override
	public CharacterStruct elt(final IntegerStruct index) {
		final int indexInt = validateIndex(index);
		return charInternal(indexInt);
	}

	@Override
	public CharacterStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}

		final int indexInt = validateIndex(index);
		return setfCharInternal((CharacterStruct) newElement, indexInt);
	}

	/**
	 * Helper method for {@link #elt(IntegerStruct)} and {@link #setfElt(LispStruct, IntegerStruct)} for validating
	 * provided index values. This is primarily here to deal with fill-pointer values, which affect the behavior of
	 * 'elt' operations in complex string structures.
	 *
	 * @param index
	 * 		the index to validate
	 *
	 * @return the validated index
	 */
	protected int validateIndex(final IntegerStruct index) {
		return validateSubscript(index);
	}
}
