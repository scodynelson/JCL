package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.ArrayDisplacement;
import jcl.lang.ArrayStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link ComplexBitVectorStructImpl} is the object representation of a Lisp 'bit-vector' type.
 */
public final class ComplexBitVectorStructImpl extends AbstractBitVectorStructImpl {

	/**
	 * The contents of the bit-vector.
	 */
	private ArrayList<FixnumStruct> contents;

	/**
	 * The fill-pointer for designating how many elements are available to be seen or consumed by certain functions.
	 */
	private IntegerStruct fillPointer;

	/**
	 * Whether or not the bit-vector is adjustable.
	 */
	private BooleanStruct adjustable;

	/**
	 * The {@link ArrayStruct} structure that this bit-vector is displaced to. If {@code null}, this structure is not
	 * displaced to anything.
	 */
	private ArrayStruct displacedTo;

	/**
	 * The index offset into the {@link #displacedTo} structure when looking for or updating elements.
	 */
	private IntegerStruct displacedIndexOffset;

	/**
	 * Public constructor, initializing the size, contents, adjustable, and fillPointer.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param contents
	 * 		the bit-vector contents
	 * @param adjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 */
	public ComplexBitVectorStructImpl(final IntegerStruct size, final List<FixnumStruct> contents,
	                                  final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		super(size);
		this.fillPointer = fillPointer;
		this.adjustable = adjustable;
		this.contents = new ArrayList<>(contents);
	}

	/**
	 * Public constructor, initializing the size, displacedTo, displacedIndexOffset, adjustable, and fillPointer.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param adjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 */
	public ComplexBitVectorStructImpl(final IntegerStruct size,
	                                  final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                                  final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		super(size);
		this.fillPointer = fillPointer;
		this.adjustable = adjustable;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public IntegerStruct fillPointer() {
		if (fillPointer == null) {
			throw new TypeErrorException("BIT-VECTOR has no fill-pointer to retrieve.");
		}
		return fillPointer;
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		if (fillPointer.isLessThan(IntegerStruct.ZERO) || fillPointer.isGreaterThan(totalSize)) {
			throw new ErrorException(
					"Fill-pointer " + fillPointer + " value is out of bounds for BIT-VECTOR with size " + totalSize + '.');
		}

		this.fillPointer = fillPointer;
		return fillPointer;
	}

	@Override
	public LispStruct vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a BIT-VECTOR with no fill-pointer.");
		}
		if (fillPointer.eql(IntegerStruct.ZERO)) {
			throw new ErrorException("Nothing left to pop.");
		}

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.subtract(IntegerStruct.ONE);

		if (displacedTo == null) {
			return contents.get(formerFillPointer.toJavaInt());
		} else {
			final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			return displacedTo.rowMajorAref(indexToGet);
		}
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a BIT-VECTOR with no fill-pointer.");
		}
		if (fillPointer.isGreaterThanOrEqualTo(totalSize)) {
			return NILStruct.INSTANCE;
		}
		final FixnumStruct newBitValue = getBit(newElement);

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.add(IntegerStruct.ONE);

		if (displacedTo == null) {
			contents.set(formerFillPointer.toJavaInt(), newBitValue);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			displacedTo.setfRowMajorAref(newBitValue, indexToSet);
		}
		return formerFillPointer;
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a BIT-VECTOR with no fill-pointer.");
		}
		if (fillPointer.isGreaterThanOrEqualTo(totalSize)) {
			if (!adjustable.toJavaPBoolean()) {
				throw new TypeErrorException("BIT-VECTOR would be extended and is not adjustable.");
			}

			final int totalSizeInt = totalSize.toJavaInt();
			final int realExtension = Math.max(extension.toJavaInt(), totalSizeInt);
			final int newTotalSize = totalSizeInt + realExtension;

			if (displacedTo == null) {
				contents.ensureCapacity(newTotalSize);
			} else {
				final List<FixnumStruct> displacedContents = getDisplacedToContents();
				contents = new ArrayList<>(totalSizeInt);
				contents.addAll(displacedContents);

				for (int index = displacedContents.size(); index < totalSizeInt; index++) {
					contents.set(index, IntegerStruct.ZERO);
				}

				displacedTo = null;
				displacedIndexOffset = null;
			}
			totalSize = IntegerStruct.toLispInteger(newTotalSize);
		}
		final FixnumStruct newBitValue = getBit(newElement);

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.add(IntegerStruct.ONE);

		if (displacedTo == null) {
			contents.set(formerFillPointer.toJavaInt(), newBitValue);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			displacedTo.setfRowMajorAref(newBitValue, indexToSet);
		}
		return formerFillPointer;
	}

	/*
	BIT-ARRAY-STRUCT
	 */

	@Override
	public FixnumStruct bit(final IntegerStruct... subscripts) {
		return aref(subscripts);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public BooleanStruct adjustableArrayP() {
		return adjustable;
	}

	@Override
	public FixnumStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		if (displacedTo == null) {
			return contents.get(rowMajorIndex.toJavaInt());
		}

		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndex);
		final LispStruct element = displacedTo.rowMajorAref(indexToGet);
		return getBit(element);
	}

	@Override
	public FixnumStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final FixnumStruct newBitValue = getBit(newElement);
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		if (displacedTo == null) {
			contents.set(rowMajorIndex.toJavaInt(), newBitValue);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndex);
			displacedTo.setfRowMajorAref(newBitValue, indexToSet);
		}
		return newBitValue;
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
	public FixnumStruct rowMajorAref(final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		if (displacedTo == null) {
			return contents.get(validIndex.toJavaInt());
		}

		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(validIndex);
		final LispStruct element = displacedTo.rowMajorAref(indexToGet);
		return getBit(element);
	}

	@Override
	public FixnumStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final FixnumStruct newBitValue = getBit(newElement);
		final IntegerStruct validIndex = validateSubscript(index);
		if (displacedTo == null) {
			contents.set(validIndex.toJavaInt(), newBitValue);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(validIndex);
			displacedTo.setfRowMajorAref(newBitValue, indexToSet);
		}
		return newBitValue;
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
	public BitVectorStruct reverse() {
		final List<FixnumStruct> reversedContents;
		if (displacedTo != null) {
			reversedContents = getDisplacedToContents();
		} else if (fillPointer == null) {
			reversedContents = new ArrayList<>(contents);
		} else {
			final List<FixnumStruct> contentsToReverse = contents.subList(0, fillPointer.toJavaInt());
			reversedContents = new ArrayList<>(contentsToReverse);
		}
		Collections.reverse(reversedContents);
		return new ComplexBitVectorStructImpl(totalSize, reversedContents, adjustable, fillPointer);
	}

	@Override
	public BitVectorStruct nReverse() {
		if (displacedTo != null) {
			final List<FixnumStruct> contentsToReverse = getDisplacedToContents();
			Collections.reverse(contentsToReverse);

			for (int index = 0; index < contentsToReverse.size(); index++) {
				final FixnumStruct indexStruct = IntegerStruct.toLispInteger(index);
				final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(indexStruct);
				final FixnumStruct content = contentsToReverse.get(index);
				displacedTo.setfRowMajorAref(content, indexToSet);
			}
		} else if (fillPointer == null) {
			Collections.reverse(contents);
		} else {
			final List<FixnumStruct> contentsToReverse = contents.subList(0, fillPointer.toJavaInt());
			Collections.reverse(contentsToReverse);
		}
		return this;
	}

	/**
	 * Returns this displaced contents value as a {@link List}.
	 *
	 * @return a {@link List} representation of the displaced contents
	 */
	private List<FixnumStruct> getDisplacedToContents() {
		final int size = (fillPointer == null) ? totalSize.toJavaInt() : fillPointer.toJavaInt();

		final List<FixnumStruct> displacedToContents = new ArrayList<>();
		for (int index = 0; index < size; index++) {
			final FixnumStruct indexStruct = IntegerStruct.toLispInteger(index);
			final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(indexStruct);

			final LispStruct current = displacedTo.rowMajorAref(indexToGet);
			final FixnumStruct currentAsBit = getBit(current);
			displacedToContents.add(currentAsBit);
		}
		return displacedToContents;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		if (displacedTo == null) {
			return new WrapperIterator<>(contents.iterator());
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
				                                Spliterator.NONNULL |
				                                Spliterator.SUBSIZED
		);
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for ComplexBitVectorStructImpl} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link #contents}, ensuring that each content {@link IntegerStruct} value is generated properly</li>
	 * <li>Constructing a new ComplexBitVectorStructImpl with the built content {@link List}</li>
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
			contentsStore = generateContents(generatorState, contents);
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
			                   GenerationConstants.BIT_VECTOR_STRUCT_NAME,
			                   GenerationConstants.BIT_VECTOR_STRUCT_TO_BIT_VECTOR_METHOD_NAME,
			                   GenerationConstants.BIT_VECTOR_STRUCT_TO_COMPLEX_BIT_VECTOR_CONTENTS_METHOD_DESC,
			                   true);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.BIT_VECTOR_STRUCT_NAME,
			                   GenerationConstants.BIT_VECTOR_STRUCT_TO_BIT_VECTOR_METHOD_NAME,
			                   GenerationConstants.BIT_VECTOR_STRUCT_TO_COMPLEX_BIT_VECTOR_DISPLACED_METHOD_DESC,
			                   true);
		}
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		final boolean printArray = CommonLispSymbols.PRINT_ARRAY_VAR.getVariableValue().toJavaPBoolean();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray) {
			stringBuilder.append("#*");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer.toJavaInt();

			for (int i = 0; i < amountToPrint; i++) {
				final IntegerStruct integerStruct = contents.get(i);
				final String printedIntegerStruct = integerStruct.toString();

				stringBuilder.append(printedIntegerStruct);
			}
		} else {
			stringBuilder.append("#<");
			stringBuilder.append(typeOf());
			stringBuilder.append(' ');

			stringBuilder.append(arrayTotalSize());

			if (fillPointer != null) {
				stringBuilder.append(" fill-pointer ");
				stringBuilder.append(fillPointer);
			}

			if (adjustable.toJavaPBoolean()) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
