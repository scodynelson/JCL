package jcl.lang.internal;

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

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.PrinterVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.SimpleStringType;
import jcl.type.StringType;
import org.apache.commons.lang3.StringUtils;

/**
 * The {@link StringStructImpl} is the object representation of a Lisp 'string' type.
 */
public final class StringStructImpl extends VectorStructImpl implements StringStruct {

	private StringBuilder contents;

	public StringStructImpl(final StringType stringType, final Integer size, final LispType elementType,
	                        final StringBuilder contents, final boolean isAdjustable,
	                        final Integer fillPointer) {
		super(stringType, size, elementType, isAdjustable, fillPointer);
		this.contents = contents;
	}

	public StringStructImpl(final StringType stringType, final Integer size, final LispType elementType,
	                        final ArrayStruct displacedTo, final Integer displacedIndexOffset,
	                        final boolean isAdjustable, final Integer fillPointer) {
		super(stringType, size, elementType, displacedTo, displacedIndexOffset, isAdjustable, fillPointer);
	}

	/*
		Old Builders
	 */

	public static StringStruct valueOf(final String stringValue) {
		return new StringStructImpl(SimpleStringType.INSTANCE,
		                            stringValue.length(),
		                            CharacterType.INSTANCE,
		                            new StringBuilder(stringValue),
		                            false,
		                            null);
	}

	/*
	STRING-STRUCT
	 */

	@Override
	public CharacterStruct char_(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return charInternal(indexInt);
	}

	private CharacterStruct charInternal(final int indexInt) {
		if (displacedTo == null) {
			final char character = contents.charAt(indexInt);
			return CharacterStructImpl.valueOf(character);
		}

		final IntegerStruct indexToGet = IntegerStructImpl.valueOf(displacedIndexOffset + indexInt);
		return (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public CharacterStruct setfChar(final CharacterStruct newElement, final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return setfCharInternal(newElement, indexInt);
	}

	private CharacterStruct setfCharInternal(final CharacterStruct newElement, final int indexInt) {
		if (displacedTo == null) {
			final char character = newElement.getCharacter();
			contents.setCharAt(indexInt, character);
		} else {
			final IntegerStruct indexToSet = IntegerStructImpl.valueOf(displacedIndexOffset + indexInt);
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
		return casifyString(context, StringUtils::capitalize);
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
		return nCasifyString(context, StringUtils::capitalize);
	}

	private StringStruct casifyString(final StringIntervalOpContext context, final Function<String, String> casifyOp) {
		final int startInt = getStringOpStart(context);
		final int endInt = getStringOpEnd(context, startInt);

		String str = contents.substring(startInt, endInt);
		str = casifyOp.apply(str);
		return valueOf(str);
	}

	private StringStruct nCasifyString(final StringIntervalOpContext context, final Function<String, String> casifyOp) {
		final int startInt = getStringOpStart(context);
		final int endInt = getStringOpEnd(context, startInt);

		String str = contents.substring(startInt, endInt);
		str = casifyOp.apply(str);
		contents.replace(startInt, endInt, str);
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

		final String str;
		if (fillPointer == null) {
			str = contents.toString();
		} else {
			str = contents.substring(0, fillPointer);
		}
		final String trimmedString = trimOp.apply(str, stripChars);
		return valueOf(trimmedString);
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

	private BooleanStruct equalComparison(final StringEqualityContext context,
	                                      final BiFunction<String, String, Integer> stringCompareToOp) {
		final EqualityStrings equalityStrings = getEqualityStrings(context);
		final String str1 = equalityStrings.getStr1();
		final String str2 = equalityStrings.getStr2();

		final int result = stringCompareToOp.apply(str1, str2);
		return LispStructFactory.toBoolean(result == 0);
	}

	private LispStruct inequalityComparison(final StringEqualityContext context,
	                                        final BiFunction<String, String, Integer> stringCompareToOp,
	                                        final IntPredicate comparisonOp,
	                                        final BiFunction<String, String, Integer> mismatchIndexOp) {
		final EqualityStrings equalityStrings = getEqualityStrings(context);
		final String str1 = equalityStrings.getStr1();
		final String str2 = equalityStrings.getStr2();

		final int result = stringCompareToOp.apply(str1, str2);
		if (comparisonOp.test(result)) {
			return NILStruct.INSTANCE;
		} else {
			final int mismatchIndex = mismatchIndexOp.apply(str1, str2);
			return IntegerStructImpl.valueOf(mismatchIndex);
		}
	}

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

	private EqualityStrings getEqualityStrings(final StringEqualityContext context) {
		final StringIntervalOpContext context1 = context.getContext1();
		final StringIntervalOpContext context2 = context.getContext2();

		final int start1 = getStringOpStart(context1);
		final int end1 = getStringOpEnd(context1, start1);

		final int start2 = getStringOpStart(context2);
		final int end2 = getStringOpEnd(context2, start2);

		final String str1;
		if (fillPointer == null) {
			str1 = contents.toString();
		} else {
			str1 = contents.substring(0, fillPointer);
		}
		final String str2 = context.getStruct()
		                           .toJavaString(true);

		final String subStr1 = str1.substring(start1, end1);
		final String subStr2 = str2.substring(start2, end2);
		return new EqualityStrings(subStr1, subStr2);
	}

	private static final class EqualityStrings {

		private final String str1;
		private final String str2;

		private EqualityStrings(final String str1, final String str2) {
			this.str1 = str1;
			this.str2 = str2;
		}

		private String getStr1() {
			return str1;
		}

		private String getStr2() {
			return str2;
		}
	}

	@Override
	public String getAsJavaString() {
		return contents.toString();
	}

	@Override
	public String toJavaString(final boolean fillPointerRestriction) {
		if (fillPointerRestriction && (fillPointer != null)) {
			return contents.substring(0, fillPointer);
		}
		return contents.toString();
	}

	private int getStringOpStart(final StringIntervalOpContext context) {
		final IntegerStruct start = context.getStart();
		final int startInt;
		if (start == null) {
			startInt = 0;
		} else {
			startInt = start.intValue();
			final int observedLength = (fillPointer == null) ? contents.length() : fillPointer;
			if ((startInt < 0) || (startInt > observedLength)) {
				throw new ErrorException(
						"Bad start value " + start + " for string with size: " + observedLength);
			}
		}
		return startInt;
	}

	private int getStringOpEnd(final StringIntervalOpContext context, final int startInt) {
		final IntegerStruct end = context.getEnd();
		final int endInt;
		if (end == null) {
			endInt = (fillPointer == null) ? contents.length() : fillPointer;
		} else {
			endInt = end.intValue();
			final int observedLength = (fillPointer == null) ? contents.length() : fillPointer;
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
	public LispStruct vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a VECTOR with no fill-pointer.");
		}
		if (fillPointer == 0) {
			throw new ErrorException("Nothing left to pop.");
		}

		fillPointer--;
		final char element = contents.charAt(fillPointer);
		return CharacterStructImpl.valueOf(element);
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (fillPointer >= contents.length()) {
			return NILStruct.INSTANCE;
		}

		final Integer previousFillPointer = fillPointer++;
		final char character = ((CharacterStruct) newElement).getCharacter();
		contents.setCharAt(fillPointer, character);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement,
	                                      final IntegerStruct extension) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (!isAdjustable) {
			throw new TypeErrorException("VECTOR is not adjustable.");
		}
		if (fillPointer >= contents.length()) {
//			adjustArray(fillPointer + extensionAmount); // TODO
		}

		final Integer previousFillPointer = fillPointer++;
		final char character = ((CharacterStruct) newElement).getCharacter();
		contents.setCharAt(fillPointer, character);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public StringStruct adjustArray(final AdjustArrayContext context) {
		return this; // TODO
	}

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return charInternal(rowMajorIndex);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}

		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return setfCharInternal((CharacterStruct) newElement, rowMajorIndex);
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return charInternal(indexInt);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
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
	public LispStruct elt(final IntegerStruct index) {
		final int indexInt = validateIndexAgainstFillPointer(index);
		return charInternal(indexInt);
	}

	@Override
	public LispStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}

		final int indexInt = validateIndexAgainstFillPointer(index);
		return setfCharInternal((CharacterStruct) newElement, indexInt);
	}

	@Override
	public SequenceStruct reverse() {
		final StringBuilder reversedContents;
		if (fillPointer == null) {
			final StringBuilder currentContents = contents;
			reversedContents = contents.reverse();
			contents = currentContents;
		} else {
			final String contentsToReverse = contents.substring(0, fillPointer);
			reversedContents = new StringBuilder(contentsToReverse).reverse();
		}
		return new StringStructImpl((StringType) getType(),
		                            totalSize,
		                            elementType,
		                            reversedContents,
		                            isAdjustable,
		                            fillPointer);
	}

	@Override
	public SequenceStruct nReverse() {
		if (fillPointer == null) {
			contents.reverse();
		} else {
			final String contentsToReverse = contents.substring(0, fillPointer);
			final String reversedContet = new StringBuilder(contentsToReverse).reverse()
			                                                                  .toString();
			contents.replace(0, fillPointer, reversedContet);
		}
		return this;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new StringStructImpl.StringIterator(contents);
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(iterator(),
		                                contents.length(),
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.IMMUTABLE |
				                                Spliterator.SUBSIZED
		);
	}

	private static final class StringIterator implements Iterator<LispStruct> {

		private final StringBuilder contents;
		private int current;

		private StringIterator(final StringBuilder contents) {
			this.contents = contents;
		}

		@Override
		public boolean hasNext() {
			try {
				contents.charAt(current);
				return true;
			} catch (final StringIndexOutOfBoundsException ignore) {
				return false;
			}
		}

		@Override
		public LispStruct next() {
			final char character;
			try {
				character = contents.charAt(current);
			} catch (final StringIndexOutOfBoundsException ex) {
				throw new NoSuchElementException(ex.getMessage());
			} finally {
				current++;
			}
			return CharacterStructImpl.valueOf(character);
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

		if (fillPointer == null) {
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
