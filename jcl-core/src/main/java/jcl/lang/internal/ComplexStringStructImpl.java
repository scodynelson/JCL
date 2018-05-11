package jcl.lang.internal;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Function;
import java.util.function.IntConsumer;

import jcl.lang.AdjustArrayContext;
import jcl.lang.ArrayStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.StringIntervalOpContext;
import jcl.lang.StringStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.CharacterConstants;
import jcl.lang.statics.PrinterVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.type.BaseCharType;
import jcl.type.BaseStringType;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.SimpleStringType;
import jcl.type.StringType;

/**
 * The {@link ComplexStringStructImpl} is the object representation of a Lisp 'string' type.
 */
public final class ComplexStringStructImpl extends AbstractStringStructImpl {

//	private CharSeq charSeq;

	/**
	 * {@link StringBuilder} containing the implementation contents of the {@link StringStruct}.
	 */
	private StringBuilder contents;

	/**
	 * The fill-pointer for designating how many elements are available to be seen or consumed by certain functions.
	 */
	private Integer fillPointer;

	/**
	 * Whether or not the {@link StringStruct} is adjustable.
	 */
	private boolean adjustable;

	/**
	 * The {@link ArrayStruct} structure that this {@link StringStruct} is displaced to. If {@code null}, this structure
	 * is not displaced to anything.
	 */
	private ArrayStruct displacedTo;

	/**
	 * The index offset into the {@link #displacedTo} structure when looking for or updating elements.
	 */
	private Integer displacedIndexOffset;

	/**
	 * Constructor for creating a new instance.
	 *
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link CharacterType} type of the elements
	 * @param contents
	 * 		the {@link StringBuilder} contents
	 * @param adjustable
	 * 		whether or not the structure is adjustable
	 * @param fillPointer
	 * 		the fill-pointer value of the structure
	 */
	public ComplexStringStructImpl(final Integer size, final CharacterType elementType, final StringBuilder contents,
	                               final boolean adjustable, final Integer fillPointer) {
		super(getStringType(elementType), elementType, size);
		this.contents = contents;
		this.fillPointer = fillPointer;
		this.adjustable = adjustable;
	}

	/**
	 * Constructor for creating a new instance.
	 *
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link CharacterType} type of the elements
	 * @param displacedTo
	 * 		the {@link ArrayStruct} structure this instance will be displaced to
	 * @param displacedIndexOffset
	 * 		the offset indicating where in the displaced to structure this structures contents will start from
	 * @param adjustable
	 * 		whether or not the structure is adjustable
	 * @param fillPointer
	 * 		the fill-pointer value of the structure
	 */
	public ComplexStringStructImpl(final Integer size, final CharacterType elementType,
	                               final ArrayStruct displacedTo, final Integer displacedIndexOffset,
	                               final boolean adjustable, final Integer fillPointer) {
		super(getStringType(elementType), elementType, size);
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
		this.fillPointer = fillPointer;
		this.adjustable = adjustable;
	}

	/**
	 * Gets the string type from the provided content element-type.
	 *
	 * @param elementType
	 * 		the string content element-type
	 *
	 * @return the matching string type for the provided content element-type
	 */
	private static StringType getStringType(final CharacterType elementType) {
		return (elementType instanceof BaseCharType) ? BaseStringType.INSTANCE : StringType.INSTANCE;
	}

	/*
	STRING-STRUCT
	 */

	@Override
	protected CharacterStruct charInternal(final int index) {
		if (displacedTo == null) {
			try {
				final char character = contents.charAt(index);
				return CharacterStruct.toLispCharacter(character);
			} catch (final StringIndexOutOfBoundsException ignored) {
				// This is here for when the 'totalSize' is more than the contents.
				// Typically will only happen with adjusted strings.
				return CharacterConstants.NULL_CHAR;
			}
		}

		final IntegerStruct indexToGet = IntegerStruct.toLispInteger(displacedIndexOffset + index);
		return (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	protected CharacterStruct setfCharInternal(final CharacterStruct newElement, final int index) {
		if (displacedTo == null) {
			final char character = newElement.toJavaChar();
			contents.setCharAt(index, character);
		} else {
			final IntegerStruct indexToSet = IntegerStruct.toLispInteger(displacedIndexOffset + index);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	@Override
	public CharacterStruct schar(final IntegerStruct index) {
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleStringType.INSTANCE + '.');
	}

	@Override
	public CharacterStruct setfSchar(final CharacterStruct newElement, final IntegerStruct index) {
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleStringType.INSTANCE + '.');
	}

	@Override
	protected StringStruct nCasifyString(final StringIntervalOpContext context,
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
				final IntegerStruct indexToGet = IntegerStruct.toLispInteger(displacedIndexOffset + index);
				final CharacterStruct character = (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
				builder.appendCodePoint(character.toUnicodeCodePoint());
			}

			String str = builder.toString();
			str = casifyOp.apply(str);

			for (int updateIndex = startInt, stringIndex = 0;
			     updateIndex < endInt;
			     updateIndex++, stringIndex++) {
				final IntegerStruct indexToSet = IntegerStruct.toLispInteger(displacedIndexOffset + updateIndex);
				final char c = str.charAt(stringIndex);
				final CharacterStruct character = CharacterStruct.toLispCharacter(c);
				displacedTo.setfRowMajorAref(character, indexToSet);
			}
		}
		return this;
	}

	@Override
	public boolean isSimpleString() {
		return false;
	}

	@Override
	public String toJavaString() {
		// TODO: right now this ignores fill-pointer by default. Should it or should it not??
		return toJavaString(true);
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
			final IntegerStruct indexToGet = IntegerStruct.toLispInteger(displacedIndexOffset + index);
			final CharacterStruct character = (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
			builder.appendCodePoint(character.toUnicodeCodePoint());
		}
		return builder.toString();
	}

	@Override
	protected int getObservedLength() {
		return (fillPointer == null) ? totalSize : fillPointer;
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public IntegerStruct fillPointer() {
		if (fillPointer == null) {
			throw new TypeErrorException("STRING has no fill-pointer to retrieve.");
		}
		return IntegerStruct.toLispInteger(fillPointer);
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		final int intValue = fillPointer.toJavaInt();
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

		final Integer formerFillPointer = fillPointer++;
		setfCharInternal((CharacterStruct) newElement, formerFillPointer);
		return IntegerStruct.toLispInteger(formerFillPointer);
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement,
	                                      final IntegerStruct extension) {
		if (!(newElement instanceof CharacterStruct)) {
			throw new TypeErrorException(newElement + " is not a character type.");
		}
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push or extend a STRING with no fill-pointer.");
		}
		if (fillPointer >= totalSize) {
			if (!adjustable) {
				throw new TypeErrorException("VECTOR would be extended and is not adjustable.");
			}
			if (displacedTo == null) {
				final int currentContentSize = contents.length();
				contents.ensureCapacity(currentContentSize + extension.toJavaInt());
				contents.setLength(currentContentSize + 1);
			} else {
				final String displacedContents = getDisplacedToAsJavaString(false);
				contents = new StringBuilder(displacedContents.length() + extension.toJavaInt());
				contents.append(displacedContents);
				contents.setLength(contents.length() + 1);

				displacedTo = null;
				displacedIndexOffset = null;
			}
		}

		final Integer formerFillPointer = fillPointer++;
		setfCharInternal((CharacterStruct) newElement, formerFillPointer);
		return IntegerStruct.toLispInteger(formerFillPointer);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	protected StringStruct adjustDisplacedTo(final AdjustArrayContext context, final LispType upgradedET) {

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final boolean newAdjustable = context.getAdjustable();
		final IntegerStruct newFillPointer = context.getFillPointer();
		final ArrayStruct newDisplacedTo = context.getDisplacedTo();
		final IntegerStruct newDisplacedIndexOffset = context.getDisplacedIndexOffset();

		final LispType displacedElementType = newDisplacedTo.arrayElementType();
		if (!upgradedET.typeEquals(displacedElementType)) {
			throw new TypeErrorException(
					"Provided array for displacement " + newDisplacedTo + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		try {
			newDisplacedTo.rowMajorAref(newDisplacedIndexOffset);
		} catch (final ErrorException ignored) {
			throw new ErrorException("Requested size is too large to displace to " + newDisplacedTo + '.');
		}

		if (adjustable) {
			totalSize = newTotalSize.toJavaInt();
			elementType = upgradedET;
			adjustable = newAdjustable;
			fillPointer = (newFillPointer == null) ? null : newFillPointer.toJavaInt();
			contents = null;
			displacedTo = newDisplacedTo;
			displacedIndexOffset = newDisplacedIndexOffset.toJavaInt();
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

	@Override
	protected StringStruct adjustInitialContents(final AdjustArrayContext context, final LispType upgradedET) {

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final SequenceStruct newInitialContents = context.getInitialContents();
		final boolean newAdjustable = context.getAdjustable();
		final IntegerStruct newFillPointer = context.getFillPointer();

		for (final LispStruct initialElement : newInitialContents) {
			final LispType currentElementType = initialElement.getType();
			if (!upgradedET.typeEquals(currentElementType)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		if (adjustable) {
			final int newTotalSizeInt = newTotalSize.toJavaInt();
			final boolean newAdjustableBoolean = newAdjustable;
			final Integer newFillPointerInt = (newFillPointer == null) ? null : newFillPointer.toJavaInt();

			final List<CharacterStruct> validContents
					= ArrayStruct.getValidContents(Collections.singletonList(newTotalSizeInt),
					                               upgradedET,
					                               newInitialContents);
			contents = validContents.stream()
			                        .mapToInt(CharacterStruct::toUnicodeCodePoint)
			                        .collect(StringBuilder::new,
			                                 StringBuilder::appendCodePoint,
			                                 StringBuilder::append);

			totalSize = newTotalSizeInt;
			elementType = upgradedET;
			adjustable = newAdjustableBoolean;
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

	@Override
	protected StringStruct adjustInitialElement(final AdjustArrayContext context, final LispType upgradedET) {
		final LispStruct newInitialElement = context.getInitialElement();
		validateNewInitialElement(newInitialElement, upgradedET);

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final int newTotalSizeInt = newTotalSize.toJavaInt();

		final boolean newAdjustable = context.getAdjustable();
		final boolean newAdjustableBoolean = newAdjustable;

		final IntegerStruct newFillPointer = context.getFillPointer();
		final Integer newFillPointerInt = (newFillPointer == null) ? null : newFillPointer.toJavaInt();

		if (adjustable) {
			if (displacedTo == null) {
				updateContentsWithElement(contents, newInitialElement, totalSize, newTotalSizeInt);
			} else {
				final String displacedContents = getDisplacedToAsJavaString(true);
				contents = new StringBuilder(displacedContents);
			}
			updateContentsWithElement(contents, newInitialElement, totalSize, newTotalSizeInt);

			totalSize = newTotalSizeInt;
			elementType = upgradedET;
			adjustable = newAdjustableBoolean;
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

			return new ComplexStringStructImpl(newTotalSizeInt,
			                                   (CharacterType) upgradedET,
			                                   newContents,
			                                   newAdjustableBoolean,
			                                   newFillPointerInt);
		}
	}

	@Override
	public boolean adjustableArrayP() {
		return adjustable;
	}

	@Override
	public boolean arrayHasFillPointerP() {
		return fillPointer != null;
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStruct.toLispInteger(displacedIndexOffset));
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public IntegerStruct length() {
		if (fillPointer != null) {
			return IntegerStruct.toLispInteger(fillPointer);
		}
		return super.length();
	}

	@Override
	protected int validateIndex(final IntegerStruct index) {
		if (fillPointer != null) {
			final int indexInt = index.toJavaInt();
			if (indexInt > fillPointer) {
				throw new ErrorException(index + " is not a valid sequence index for " + this);
			}
		}
		return super.validateIndex(index);
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
		return new ComplexStringStructImpl(totalSize,
		                                   (CharacterType) elementType,
		                                   reversedContents,
		                                   adjustable,
		                                   fillPointer);
	}

	@Override
	public StringStruct nReverse() {
		if (displacedTo != null) {
			final String contentsToReverse = getDisplacedToAsJavaString(false);
			final StringBuilder reversedContent = new StringBuilder(contentsToReverse).reverse();

			for (int index = 0; index < reversedContent.length(); index++) {
				final IntegerStruct indexToSet = IntegerStruct.toLispInteger(displacedIndexOffset + index);
				final char c = reversedContent.charAt(index);
				final CharacterStruct character = CharacterStruct.toLispCharacter(c);
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
			return new ComplexStringStructImpl.StringIterator(contents);
		} else {
			return new ComplexStringStructImpl.DisplacedStringIterator(totalSize, displacedTo, displacedIndexOffset);
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
		 * The contents of the {@link ComplexStringStructImpl} being iterated over.
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
		 * 		the contents of the {@link ComplexStringStructImpl} to be iterated over
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
			return CharacterStruct.toLispCharacter(character);
		}
	}

	/**
	 * Iterator for {@link StringStruct} structures with displaced contents.
	 */
	private static final class DisplacedStringIterator implements Iterator<LispStruct> {

		/**
		 * The total size of the contents of the {@link ComplexStringStructImpl} structure.
		 */
		private final int totalSize;

		/**
		 * The {@link ArrayStruct} the {@link ComplexStringStructImpl} is displaced to.
		 */
		private final ArrayStruct displacedTo;

		/**
		 * The offset value into the displaced array structure where the {@link ComplexStringStructImpl} starts.
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
		 * 		the total size of the contents of the {@link ComplexStringStructImpl} structure
		 * @param displacedTo
		 * 		the {@link ArrayStruct} the {@link ComplexStringStructImpl} is displaced to
		 * @param displacedIndexOffset
		 * 		the offset value into the displaced array structure where the {@link ComplexStringStructImpl} starts
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
				final IntegerStruct indexToGet = IntegerStruct.toLispInteger(displacedIndexOffset + current);
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
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue().toJavaPBoolean();

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
