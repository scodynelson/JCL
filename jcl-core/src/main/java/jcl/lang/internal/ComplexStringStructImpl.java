package jcl.lang.internal;

import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Function;
import java.util.function.IntConsumer;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.ArrayDisplacement;
import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.SyntaxType;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CharacterConstants;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link ComplexStringStructImpl} is the object representation of a Lisp 'string' type.
 */
public final class ComplexStringStructImpl extends AbstractStringStructImpl {

	/**
	 * {@link StringBuilder} containing the implementation contents of the {@link StringStruct}.
	 */
	private StringBuilder contents;

	/**
	 * The fill-pointer for designating how many elements are available to be seen or consumed by certain functions.
	 */
	private IntegerStruct fillPointer;

	/**
	 * Whether or not the {@link StringStruct} is adjustable.
	 */
	private BooleanStruct adjustable;

	/**
	 * The {@link ArrayStruct} structure that this {@link StringStruct} is displaced to. If {@code null}, this structure
	 * is not displaced to anything.
	 */
	private ArrayStruct displacedTo;

	/**
	 * The index offset into the {@link #displacedTo} structure when looking for or updating elements.
	 */
	private IntegerStruct displacedIndexOffset;

	/**
	 * Public constructor, initializing the size, element-type, contents, adjustable, and fillPointer.
	 *
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link LispStruct} type of the elements
	 * @param contents
	 * 		the {@link StringBuilder} contents
	 * @param adjustable
	 * 		whether or not the structure is adjustable
	 * @param fillPointer
	 * 		the fill-pointer value of the structure
	 */
	public ComplexStringStructImpl(final IntegerStruct size, final LispStruct elementType, final StringBuilder contents,
	                               final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		super(elementType, size);
		this.fillPointer = fillPointer;
		this.adjustable = adjustable;
		this.contents = contents;
	}

	/**
	 * Public constructor, initializing the size, element-type, displacedTo, displacedIndexOffset, adjustable, and
	 * fillPointer.
	 *
	 * @param size
	 * 		the size of the structure
	 * @param elementType
	 * 		the {@link LispStruct} type of the elements
	 * @param displacedTo
	 * 		the {@link ArrayStruct} structure this instance will be displaced to
	 * @param displacedIndexOffset
	 * 		the offset indicating where in the displaced to structure this structures contents will start from
	 * @param adjustable
	 * 		whether or not the structure is adjustable
	 * @param fillPointer
	 * 		the fill-pointer value of the structure
	 */
	public ComplexStringStructImpl(final IntegerStruct size, final LispStruct elementType,
	                               final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                               final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		super(elementType, size);
		this.fillPointer = fillPointer;
		this.adjustable = adjustable;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	/*
	STRING-STRUCT
	 */

	@Override
	protected StringStruct nCasifyString(final StringIntervalOpContext context,
	                                     final Function<String, String> casifyOp) {
		final IntegerStruct start = getStringOpStart(this, context);
		final IntegerStruct end = getStringOpEnd(this, context, start);

		final int startInt = start.toJavaInt();
		final int endInt = end.toJavaInt();

		if (displacedTo == null) {
			String str = contents.substring(startInt, endInt);
			str = casifyOp.apply(str);
			contents.replace(startInt, endInt, str);
		} else {
			final StringBuilder builder = new StringBuilder();
			for (int index = startInt; index < endInt; index++) {
				// TODO: Check Casting
				final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(IntegerStruct.toLispInteger(index));
				final CharacterStruct character = (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
				builder.appendCodePoint(character.toUnicodeCodePoint());
			}

			String str = builder.toString();
			str = casifyOp.apply(str);

			for (int updateIndex = startInt, stringIndex = 0;
			     updateIndex < endInt;
			     updateIndex++, stringIndex++) {
				// TODO: Check Casting
				final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(
						IntegerStruct.toLispInteger(updateIndex));
				final char c = str.charAt(stringIndex);
				final CharacterStruct character = CharacterStruct.toLispCharacter(c);
				displacedTo.setfRowMajorAref(character, indexToSet);
			}
		}
		return this;
	}

	@Override
	public BooleanStruct isSimpleString() {
		return NILStruct.INSTANCE;
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
			return contents.substring(0, fillPointer.toJavaInt());
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
		final int size = (!ignoreFillPointer && (fillPointer != null))
		                 ? fillPointer.toJavaInt()
		                 : totalSize.toJavaInt();
		final StringBuilder builder = new StringBuilder();
		for (int index = 0; index < size; index++) {
			// TODO: Check Casting
			final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(
					IntegerStruct.toLispInteger(index));
			final CharacterStruct character = (CharacterStruct) displacedTo.rowMajorAref(indexToGet);
			builder.appendCodePoint(character.toUnicodeCodePoint());
		}
		return builder.toString();
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public IntegerStruct fillPointer() {
		if (fillPointer == null) {
			throw new TypeErrorException("STRING has no fill-pointer to retrieve.");
		}
		return fillPointer;
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		if (fillPointer.isLessThan(IntegerStruct.ZERO) || fillPointer.isGreaterThan(totalSize)) {
			throw new ErrorException(
					"Fill-pointer " + fillPointer + " value is out of bounds for STRING with size " + totalSize + '.');
		}

		this.fillPointer = fillPointer;
		return fillPointer;
	}

	@Override
	public CharacterStruct vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a STRING with no fill-pointer.");
		}
		if (fillPointer.eql(IntegerStruct.ZERO)) {
			throw new ErrorException("Nothing left to pop.");
		}

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.subtract(IntegerStruct.ONE);

		if (displacedTo == null) {
			return getCharacterFromContents(formerFillPointer);
		} else {
			final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			final LispStruct element = displacedTo.rowMajorAref(indexToGet);
			return getCharacter(element);
		}
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a STRING with no fill-pointer.");
		}
		if (fillPointer.isGreaterThanOrEqualTo(totalSize)) {
			return NILStruct.INSTANCE;
		}
		final CharacterStruct newCharacterValue = getCharacter(newElement);

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.add(IntegerStruct.ONE);

		if (displacedTo == null) {
			contents.setCharAt(formerFillPointer.toJavaInt(), newCharacterValue.toJavaChar());
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			displacedTo.setfRowMajorAref(newCharacterValue, indexToSet);
		}
		return formerFillPointer;
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push or extend a STRING with no fill-pointer.");
		}
		if (fillPointer.isGreaterThanOrEqualTo(totalSize)) {
			if (!adjustable.toJavaPBoolean()) {
				throw new TypeErrorException("STRING would be extended and is not adjustable.");
			}

			final int totalSizeInt = totalSize.toJavaInt();
			final int realExtension = Math.max(extension.toJavaInt(), totalSizeInt);
			final int newTotalSize = totalSizeInt + realExtension;

			if (displacedTo == null) {
				contents.setLength(newTotalSize);
			} else {
				final String displacedContents = getDisplacedToAsJavaString(false);
				contents = new StringBuilder(displacedContents);
				contents.setLength(newTotalSize);

				displacedTo = null;
				displacedIndexOffset = null;
			}
			totalSize = IntegerStruct.toLispInteger(newTotalSize);
		}
		final CharacterStruct newCharacterValue = getCharacter(newElement);

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.add(IntegerStruct.ONE);

		if (displacedTo == null) {
			contents.setCharAt(formerFillPointer.toJavaInt(), newCharacterValue.toJavaChar());
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			displacedTo.setfRowMajorAref(newCharacterValue, indexToSet);
		}
		return formerFillPointer;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public BooleanStruct adjustableArrayP() {
		return adjustable;
	}

	@Override
	public CharacterStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		if (displacedTo == null) {
			return getCharacterFromContents(rowMajorIndex);
		}

		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndex);
		final LispStruct element = displacedTo.rowMajorAref(indexToGet);
		return getCharacter(element);
	}

	@Override
	public CharacterStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final CharacterStruct newCharacterValue = getCharacter(newElement);
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		if (displacedTo == null) {
			contents.setCharAt(rowMajorIndex.toJavaInt(), newCharacterValue.toJavaChar());
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndex);
			displacedTo.setfRowMajorAref(newCharacterValue, indexToSet);
		}
		return newCharacterValue;
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return BooleanStruct.toLispBoolean(fillPointer != null);
	}

	@Override
	public ArrayDisplacement arrayDisplacement() {
		return new ArrayDisplacement(displacedTo, displacedIndexOffset);
	}

	@Override
	public CharacterStruct rowMajorAref(final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		if (displacedTo == null) {
			return getCharacterFromContents(validIndex);
		}

		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(validIndex);
		final LispStruct element = displacedTo.rowMajorAref(indexToGet);
		return getCharacter(element);
	}

	@Override
	public CharacterStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final CharacterStruct newCharacterValue = getCharacter(newElement);
		final IntegerStruct validIndex = validateSubscript(index);
		if (displacedTo == null) {
			contents.setCharAt(validIndex.toJavaInt(), newCharacterValue.toJavaChar());
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(validIndex);
			displacedTo.setfRowMajorAref(newCharacterValue, indexToSet);
		}
		return newCharacterValue;
	}

	/**
	 * Retrieves the {@link CharacterStruct} from the {@link #contents} at the provided index location. If the index is
	 * out of bounds, {@link CharacterConstants#NULL_CHAR} is returned.
	 *
	 * @param index
	 * 		the index of the {@link CharacterStruct} to retrieve
	 *
	 * @return the {@link CharacterStruct} from the {@link #contents} at the provided index location
	 */
	private CharacterStruct getCharacterFromContents(final IntegerStruct index) {
		try {
			final char character = contents.charAt(index.toJavaInt());
			return CharacterStruct.toLispCharacter(character);
		} catch (final StringIndexOutOfBoundsException ignored) {
			// This is here for when the 'totalSize' is more than the contents.
			// Typically will only happen with adjusted strings.
			return CharacterConstants.NULL_CHAR;
		}
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public IntegerStruct length() {
		return (fillPointer == null) ? totalSize : fillPointer;
	}

	@Override
	protected IntegerStruct validateIndex(final IntegerStruct index) {
		if (fillPointer != null) {
			if (index.isGreaterThan(fillPointer)) {
				throw new ErrorException(index + " is not a valid sequence index for " + this);
			}
		}
		return super.validateIndex(index);
	}

	@Override
	public StringStruct reverse() {
		final String contentsToReverse;
		if (displacedTo != null) {
			contentsToReverse = getDisplacedToAsJavaString(false);
		} else if (fillPointer == null) {
			contentsToReverse = contents.toString();
		} else {
			contentsToReverse = contents.substring(0, fillPointer.toJavaInt());
		}
		final StringBuilder reversedContents = new StringBuilder(contentsToReverse).reverse();
		return new ComplexStringStructImpl(totalSize, elementType, reversedContents, adjustable, fillPointer);
	}

	@Override
	public StringStruct nReverse() {
		if (displacedTo != null) {
			final String contentsToReverse = getDisplacedToAsJavaString(false);
			final StringBuilder reversedContent = new StringBuilder(contentsToReverse).reverse();

			for (int index = 0; index < reversedContent.length(); index++) {
				final FixnumStruct indexStruct = IntegerStruct.toLispInteger(index);
				final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(indexStruct);
				final CharacterStruct character = CharacterStruct.toLispCharacter(reversedContent.charAt(index));
				displacedTo.setfRowMajorAref(character, indexToSet);
			}
		} else if (fillPointer == null) {
			contents.reverse();
		} else {
			final int fillPointerInt = fillPointer.toJavaInt();

			final String contentsToReverse = contents.substring(0, fillPointerInt);
			final String reversedContent = new StringBuilder(contentsToReverse).reverse().toString();
			contents.replace(0, fillPointerInt, reversedContent);
		}
		return this;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		if (displacedTo == null) {
			return new StringIterator(contents);
		} else {
			return new DisplacedArrayIterator(totalSize, displacedTo, displacedIndexOffset);
		}
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(iterator(),
		                                totalSize.toJavaInt(),
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.SUBSIZED
		);
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for ComplexStringStructImpl} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link #contents}, ensuring that each content {@link CharacterStruct} value is generated properly</li>
	 * <li>Constructing a new ComplexStringStructImpl with the built content {@link List}</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int sizeStore = generateSize(generatorState);

		int contentsStore = 0;
		int displacedToStore = 0;
		int displacedIndexOffsetStore = 0;
		if (displacedTo == null) {
			contentsStore = methodBuilder.getNextAvailableStore();

			final String javaString = contents.toString();
			mv.visitLdcInsn(javaString);
			mv.visitVarInsn(Opcodes.ASTORE, contentsStore);
		} else {
			displacedToStore = methodBuilder.getNextAvailableStore();
			displacedTo.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, displacedToStore);

			displacedIndexOffsetStore = methodBuilder.getNextAvailableStore();
			displacedIndexOffset.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, displacedIndexOffsetStore);
		}

		final int adjustableStore = methodBuilder.getNextAvailableStore();
		adjustable.generate(generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, adjustableStore);

		final int fillPointerStore = methodBuilder.getNextAvailableStore();
		if (fillPointer == null) {
			mv.visitInsn(Opcodes.ACONST_NULL);
		} else {
			fillPointer.generate(generatorState);
		}
		mv.visitVarInsn(Opcodes.ASTORE, fillPointerStore);

		mv.visitVarInsn(Opcodes.ALOAD, sizeStore);

		if (displacedTo == null) {
			mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		} else {
			mv.visitVarInsn(Opcodes.ALOAD, displacedToStore);
			mv.visitVarInsn(Opcodes.ALOAD, displacedIndexOffsetStore);
		}

		mv.visitVarInsn(Opcodes.ALOAD, adjustableStore);
		mv.visitVarInsn(Opcodes.ALOAD, fillPointerStore);

		if (displacedTo == null) {
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.STRING_STRUCT_NAME,
			                   GenerationConstants.STRING_STRUCT_TO_LISP_STRING_METHOD_NAME,
			                   GenerationConstants.STRING_STRUCT_TO_COMPLEX_STRING_CONTENTS_METHOD_DESC,
			                   true);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.STRING_STRUCT_NAME,
			                   GenerationConstants.STRING_STRUCT_TO_LISP_STRING_METHOD_NAME,
			                   GenerationConstants.STRING_STRUCT_TO_COMPLEX_STRING_DISPLACED_METHOD_DESC,
			                   true);
		}
	}

	@Override
	public LispStruct typeOf() {
		if (CommonLispSymbols.BASE_CHAR == elementType) {
			return ListStruct.toLispList(CommonLispSymbols.BASE_STRING, totalSize);
		} else {
			return ListStruct.toLispList(CommonLispSymbols.STRING, totalSize);
		}
	}

	@Override
	public ClassStruct classOf() {
		if (CommonLispSymbols.BASE_CHAR == elementType) {
			return BuiltInClassStruct.BASE_STRING;
		} else {
			return BuiltInClassStruct.STRING;
		}
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		final boolean printEscape = CommonLispSymbols.PRINT_ESCAPE_VAR.getVariableValue().toJavaPBoolean();

		final ReadtableStruct readtable = CommonLispSymbols.READTABLE_VAR.getVariableValue();

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
			        .limit(fillPointer.toJavaInt())
			        .forEach(appendFn);
		}

		if (printEscape) {
			stringBuilder.append('"');
		}

		return stringBuilder.toString();
	}
}
