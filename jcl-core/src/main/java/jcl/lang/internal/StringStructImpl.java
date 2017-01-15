package jcl.lang.internal;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.IntConsumer;

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
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.PrinterVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.SimpleStringType;
import jcl.type.StringType;

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
	public StringStruct stringUpcase(final StringCaseContext context) {
		return null;
	}

	@Override
	public StringStruct stringDowncase(final StringCaseContext context) {
		return null;
	}

	@Override
	public StringStruct stringCapitalize(final StringCaseContext context) {
		return null;
	}

	@Override
	public StringStruct nStringUpcase(final StringCaseContext context) {
		return null;
	}

	@Override
	public StringStruct nStringDowncase(final StringCaseContext context) {
		return null;
	}

	@Override
	public StringStruct nStringCapitalize(final StringCaseContext context) {
		return null;
	}

	@Override
	public StringStruct stringTrim(final SequenceStruct characterBag) {
		return null;
	}

	@Override
	public StringStruct stringLeftTrim(final SequenceStruct characterBag) {
		return null;
	}

	@Override
	public StringStruct stringRightTrim(final SequenceStruct characterBag) {
		return null;
	}

	@Override
	public BooleanStruct stringEqual(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringNotEqual(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringLessThan(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringGreaterThan(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringLessThanOrEqualTo(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringGreaterThanOrEqualTo(final StringEqualityContext context) {
		return null;
	}

	@Override
	public BooleanStruct stringEqualIgnoreCase(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringNotEqualIgnoreCase(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringLessThanIgnoreCase(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringGreaterThanIgnoreCase(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringLessThanOrEqualToIgnoreCase(final StringEqualityContext context) {
		return null;
	}

	@Override
	public IntegerStruct stringGreaterThanOrEqualToIgnoreCase(final StringEqualityContext context) {
		return null;
	}

	@Override
	public String getAsJavaString() {
		return contents.toString();
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
