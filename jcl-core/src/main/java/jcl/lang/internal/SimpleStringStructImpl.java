package jcl.lang.internal;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Function;
import java.util.function.IntConsumer;

import jcl.lang.AdjustArrayContext;
import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
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
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.SimpleBaseStringType;
import jcl.type.SimpleStringType;
import jcl.type.StringType;

/**
 * The {@link SimpleStringStructImpl} is the object representation of a Lisp 'string' type.
 */
public final class SimpleStringStructImpl extends AbstractStringStructImpl {

	/**
	 * {@link StringBuilder} containing the implementation contents of the {@link StringStruct}.
	 */
	private StringBuilder contents;

	/**
	 * Constructor for creating a new instance.
	 *
	 * @param str
	 * 		the Java {@link String} to wrap
	 */
	public SimpleStringStructImpl(final String str) {
		super(SimpleStringType.INSTANCE, CharacterType.INSTANCE, str.length());
		contents = new StringBuilder(str);
	}

	/**
	 * Constructor for creating a new instance.
	 *
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link CharacterType} type of the elements
	 * @param contents
	 * 		the {@link StringBuilder} contents
	 */
	public SimpleStringStructImpl(final Integer size, final CharacterType elementType, final StringBuilder contents) {
		super(getStringType(elementType), elementType, size);
		this.contents = contents;
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
		return (elementType instanceof BaseCharType) ? SimpleBaseStringType.INSTANCE : SimpleStringType.INSTANCE;
	}

	/*
	STRING-STRUCT
	 */

	@Override
	protected CharacterStruct charInternal(final int index) {
		try {
			final char character = contents.charAt(index);
			return CharacterStruct.toLispCharacter(character);
		} catch (final StringIndexOutOfBoundsException ignored) {
			// This is here for when the 'totalSize' is more than the contents.
			// Typically will only happen with adjusted strings.
			return CharacterConstants.NULL_CHAR;
		}
	}

	@Override
	protected CharacterStruct setfCharInternal(final CharacterStruct newElement, final int index) {
		final char character = newElement.toJavaChar();
		contents.setCharAt(index, character);
		return newElement;
	}

	@Override
	public CharacterStruct schar(final IntegerStruct index) {
		return char_(index);
	}

	@Override
	public CharacterStruct setfSchar(final CharacterStruct newElement, final IntegerStruct index) {
		return setfChar(newElement, index);
	}

	@Override
	protected StringStruct nCasifyString(final StringIntervalOpContext context,
	                                     final Function<String, String> casifyOp) {
		final int startInt = getStringOpStart(context);
		final int endInt = getStringOpEnd(context, startInt);

		String str = contents.substring(startInt, endInt);
		str = casifyOp.apply(str);
		contents.replace(startInt, endInt, str);
		return this;
	}

	@Override
	public BooleanStruct isSimpleString() {
		return BooleanStruct.T;
	}

	@Override
	public String toJavaString() {
		return contents.toString();
	}

	@Override
	public String toJavaString(final boolean ignoreFillPointer) {
		return contents.toString();
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public IntegerStruct fillPointer() {
		throw new TypeErrorException("STRING has no fill-pointer to retrieve.");
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		throw new TypeErrorException("Cannot set fill-pointer for SIMPLE-STRING.");
	}

	@Override
	public CharacterStruct vectorPop() {
		throw new TypeErrorException("Cannot pop from a STRING with no fill-pointer.");
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		throw new TypeErrorException("Cannot push into a STRING with no fill-pointer.");
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		throw new TypeErrorException("Cannot push or extend a STRING with no fill-pointer.");
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	protected StringStruct adjustDisplacedTo(final AdjustArrayContext context, final LispType upgradedET) {

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final BooleanStruct newAdjustable = context.getAdjustable();
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

		return StringStruct.builder(newTotalSize)
		                   .elementType((CharacterType) upgradedET)
		                   .adjustable(newAdjustable)
		                   .fillPointer(newFillPointer)
		                   .displacedTo(newDisplacedTo)
		                   .displacedIndexOffset(newDisplacedIndexOffset)
		                   .build();
	}

	@Override
	protected StringStruct adjustInitialContents(final AdjustArrayContext context, final LispType upgradedET) {

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final SequenceStruct newInitialContents = context.getInitialContents();
		final BooleanStruct newAdjustable = context.getAdjustable();
		final IntegerStruct newFillPointer = context.getFillPointer();

		for (final LispStruct initialElement : newInitialContents) {
			final LispType currentElementType = initialElement.getType();
			if (!upgradedET.typeEquals(currentElementType)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		return StringStruct.builder(newTotalSize)
		                   .elementType((CharacterType) upgradedET)
		                   .adjustable(newAdjustable)
		                   .fillPointer(newFillPointer)
		                   .initialContents(newInitialContents)
		                   .build();
	}

	@Override
	protected StringStruct adjustInitialElement(final AdjustArrayContext context, final LispType upgradedET) {
		final LispStruct newInitialElement = context.getInitialElement();
		validateNewInitialElement(newInitialElement, upgradedET);

		final IntegerStruct newTotalSize = context.getDimensions().get(0);
		final int newTotalSizeInt = newTotalSize.intValue();

		final BooleanStruct newAdjustable = context.getAdjustable();
		final boolean newAdjustableBoolean = newAdjustable.booleanValue();

		final IntegerStruct newFillPointer = context.getFillPointer();
		final Integer newFillPointerInt = (newFillPointer == null) ? null : newFillPointer.intValue();

		final StringBuilder newContents = new StringBuilder(contents);
		updateContentsWithElement(newContents, newInitialElement, totalSize, newTotalSizeInt);

		if ((newFillPointerInt == null) && !newAdjustableBoolean) {
			return new SimpleStringStructImpl(newTotalSizeInt,
			                                  (CharacterType) upgradedET,
			                                  newContents);
		}
		return new ComplexStringStructImpl(newTotalSizeInt,
		                                   (CharacterType) upgradedET,
		                                   newContents,
		                                   newAdjustableBoolean,
		                                   newFillPointerInt);
	}

	@Override
	public BooleanStruct adjustableArrayP() {
		return BooleanStruct.NIL;
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return BooleanStruct.NIL;
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO);
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public StringStruct reverse() {
		final String contentsToReverse = contents.toString();
		final StringBuilder reversedContents = new StringBuilder(contentsToReverse).reverse();
		return new SimpleStringStructImpl(totalSize, (CharacterType) elementType, reversedContents);
	}

	@Override
	public StringStruct nReverse() {
		contents.reverse();
		return this;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new SimpleStringStructImpl.StringIterator(contents);
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
		 * The contents of the {@link SimpleStringStructImpl} being iterated over.
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
		 * 		the contents of the {@link SimpleStringStructImpl} to be iterated over
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

		contents.codePoints()
		        .forEach(appendFn);

		if (printEscape) {
			stringBuilder.append('"');
		}

		return stringBuilder.toString();
	}
}
