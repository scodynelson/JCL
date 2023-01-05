package jcl.lang.internal;

import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.IntPredicate;
import java.util.stream.Collectors;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.WordUtils;

/**
 * Base class for all {@link StringStruct} implementation structures.
 */
public abstract class AbstractStringStructImpl extends AbstractVectorStructImpl implements StringStruct {

	/**
	 * Protected constructor for initializing {@link #elementType}, and {@link #totalSize} properties.
	 *
	 * @param elementType
	 * 		the value used to initialize {@link #elementType} property
	 * @param totalSize
	 * 		the value used to initialize {@link #totalSize} property
	 */
	protected AbstractStringStructImpl(final LispStruct elementType, final IntegerStruct totalSize) {
		super(elementType, totalSize);
	}

	/**
	 * Validates and returns the provided object as a 'character' value.
	 *
	 * @param object
	 * 		the object to validate
	 *
	 * @return the provided object as a 'character' value
	 *
	 * @throws TypeErrorException
	 * 		if the provided object is not a valid 'character' value
	 */
	protected static CharacterStruct getCharacter(final LispStruct object) {
		if (object instanceof CharacterStruct) {
			return (CharacterStruct) object;
		}
		throw new TypeErrorException(object + " is not a character type.");
	}

	/*
	STRING-STRUCT
	 */

	@Override
	public StringStruct stringUpcase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return casifyString(context, String::toUpperCase);
	}

	@Override
	public StringStruct stringDowncase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return casifyString(context, String::toLowerCase);
	}

	@Override
	public StringStruct stringCapitalize(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return casifyString(context, WordUtils::capitalize);
	}

	@Override
	public StringStruct nStringUpcase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return nCasifyString(context, String::toUpperCase);
	}

	@Override
	public StringStruct nStringDowncase(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
		return nCasifyString(context, String::toLowerCase);
	}

	@Override
	public StringStruct nStringCapitalize(final IntegerStruct start, final IntegerStruct end) {
		final StringIntervalOpContext context
				= StringIntervalOpContext.builder()
				                         .start(start)
				                         .end(end)
				                         .build();
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
		final IntegerStruct start = getStringOpStart(this, context);
		final IntegerStruct end = getStringOpEnd(this, context, start);

		final String str = toJavaString(false);
		final StringBuilder builder = new StringBuilder(str);

		final int startInt = start.toJavaInt();
		final int endInt = end.toJavaInt();

		String strToCasify = builder.substring(startInt, endInt);
		strToCasify = casifyOp.apply(strToCasify);
		builder.replace(startInt, endInt, strToCasify);

		return new SimpleStringStructImpl(IntegerStruct.toLispInteger(builder.length()), elementType, builder);
	}

	/**
	 * Destructively modifies the current contents or possible displaced contents by utilizing the provided
	 * case-altering operation function and the provided {@link StringIntervalOpContext} for the start and end values in
	 * determining casing boundaries.
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
		return new SimpleStringStructImpl(IntegerStruct.toLispInteger(trimmedString.length()),
		                                  elementType,
		                                  new StringBuilder(trimmedString));
	}

	@Override
	public BooleanStruct stringEqual(final StringStruct string2,
	                                 final IntegerStruct start1, final IntegerStruct end1,
	                                 final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return equalComparison(context,
		                       String::compareTo);
	}

	@Override
	public LispStruct stringNotEqual(final StringStruct string2,
	                                 final IntegerStruct start1, final IntegerStruct end1,
	                                 final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x != 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringLessThan(final StringStruct string2,
	                                 final IntegerStruct start1, final IntegerStruct end1,
	                                 final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x < 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThan(final StringStruct string2,
	                                    final IntegerStruct start1, final IntegerStruct end1,
	                                    final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x > 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringLessThanOrEqualTo(final StringStruct string2,
	                                          final IntegerStruct start1, final IntegerStruct end1,
	                                          final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x <= 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThanOrEqualTo(final StringStruct string2,
	                                             final IntegerStruct start1, final IntegerStruct end1,
	                                             final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareTo,
		                            x -> x >= 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public BooleanStruct stringEqualIgnoreCase(final StringStruct string2,
	                                           final IntegerStruct start1, final IntegerStruct end1,
	                                           final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return equalComparison(context,
		                       String::compareToIgnoreCase);
	}

	@Override
	public LispStruct stringNotEqualIgnoreCase(final StringStruct string2,
	                                           final IntegerStruct start1, final IntegerStruct end1,
	                                           final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x != 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringLessThanIgnoreCase(final StringStruct string2,
	                                           final IntegerStruct start1, final IntegerStruct end1,
	                                           final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x < 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThanIgnoreCase(final StringStruct string2,
	                                              final IntegerStruct start1, final IntegerStruct end1,
	                                              final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x > 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringLessThanOrEqualToIgnoreCase(final StringStruct string2,
	                                                    final IntegerStruct start1, final IntegerStruct end1,
	                                                    final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x <= 0,
		                            StringUtils::indexOfDifference);
	}

	@Override
	public LispStruct stringGreaterThanOrEqualToIgnoreCase(final StringStruct string2,
	                                                       final IntegerStruct start1, final IntegerStruct end1,
	                                                       final IntegerStruct start2, final IntegerStruct end2) {
		final StringEqualityContext context =
				StringEqualityContext.builder(string2)
				                     .start1(start1).end1(end1)
				                     .start2(start2).end2(end2)
				                     .build();
		return inequalityComparison(context,
		                            String::compareToIgnoreCase,
		                            x -> x >= 0,
		                            StringUtils::indexOfDifference);
	}

	/**
	 * Compares this string using the provided comparison function with the {@link StringEqualityContext#getStruct()}
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
		return BooleanStruct.toLispBoolean(result == 0);
	}

	/**
	 * Compares this string using the provided comparison function with the {@link StringEqualityContext#getStruct()}
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
		final StringStruct struct = context.getStruct();
		final StringIntervalOpContext context1 = context.getContext1();
		final StringIntervalOpContext context2 = context.getContext2();

		final IntegerStruct start1 = getStringOpStart(this, context1);
		final IntegerStruct end1 = getStringOpEnd(this, context1, start1);

		final IntegerStruct start2 = getStringOpStart(struct, context2);
		final IntegerStruct end2 = getStringOpEnd(struct, context2, start2);

		final String str1 = toJavaString(false);
		final String str2 = struct.toJavaString(false);

		final String subStr1 = str1.substring(start1.toJavaInt(), end1.toJavaInt());
		final String subStr2 = str2.substring(start2.toJavaInt(), end2.toJavaInt());
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
	 * @param struct
	 * 		the string structure to find the starting value for
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing starting and ending values for string operations
	 *
	 * @return the start value for the string operation
	 */
	protected static IntegerStruct getStringOpStart(final StringStruct struct, final StringIntervalOpContext context) {
		final IntegerStruct start = context.getStart();
		final IntegerStruct realStart;
		if (start == null) {
			realStart = IntegerStruct.ZERO;
		} else {
			realStart = start;
			final IntegerStruct observedLength = struct.length();
			if (start.isLessThan(IntegerStruct.ZERO) || start.isGreaterThan(observedLength)) {
				throw new ErrorException(
						"Bad start value " + start + " for string with size: " + observedLength);
			}
		}
		return realStart;
	}

	/**
	 * Retrieves the ending value from the {@link StringIntervalOpContext} for a string operation.
	 *
	 * @param struct
	 * 		the string structure to find the ending value for
	 * @param context
	 * 		the {@link StringIntervalOpContext} containing starting and ending values for string operations
	 * @param start
	 * 		the starting value for the string operation
	 *
	 * @return the end value for the string operation
	 */
	protected static IntegerStruct getStringOpEnd(final StringStruct struct, final StringIntervalOpContext context,
	                                              final IntegerStruct start) {
		final IntegerStruct end = context.getEnd();
		final IntegerStruct realEnd;
		if (end == null) {
			realEnd = struct.length();
		} else {
			realEnd = end;
			final IntegerStruct observedLength = struct.length();
			if (end.isLessThan(IntegerStruct.ZERO) || end.isGreaterThan(observedLength) || end.isLessThan(start)) {
				throw new ErrorException(
						"Bad end value " + end + " with start value " + start + " for string with size: " + observedLength);
			}
		}
		return realEnd;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public abstract CharacterStruct aref(final IntegerStruct... subscripts);

	@Override
	public abstract CharacterStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts);

	@Override
	public abstract CharacterStruct rowMajorAref(final IntegerStruct index);

	@Override
	public abstract CharacterStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index);

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public CharacterStruct elt(final IntegerStruct index) {
		final IntegerStruct validIndex = validateIndex(index);
		return aref(validIndex);
	}

	@Override
	public CharacterStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		final IntegerStruct validIndex = validateIndex(index);
		return setfAref(newElement, validIndex);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if ((typeSpecifier == CommonLispSymbols.BASE_STRING) && (CommonLispSymbols.BASE_CHAR == elementType)) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.STRING) {
			return TStruct.INSTANCE;
		}
		if ((typeSpecifier == BuiltInClassStruct.BASE_STRING) && (CommonLispSymbols.BASE_CHAR == elementType)) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.STRING) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
