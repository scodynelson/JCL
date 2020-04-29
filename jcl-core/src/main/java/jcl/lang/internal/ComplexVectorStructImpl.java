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
import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RealStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.PrinterVariables;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link ComplexVectorStructImpl} is the object representation of a Lisp 'vector' type.
 */
public class ComplexVectorStructImpl extends AbstractVectorStructImpl {

	/**
	 * The contents of the vector.
	 */
	private ArrayList<LispStruct> contents;

	/**
	 * The fill-pointer for designating how many elements are available to be seen or consumed by certain functions.
	 */
	private IntegerStruct fillPointer;

	/**
	 * Whether or not the vector is adjustable.
	 */
	private BooleanStruct adjustable;

	/**
	 * The {@link ArrayStruct} structure that this vector is displaced to. If {@code null}, this structure is not
	 * displaced to anything.
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
	 * 		the vector size
	 * @param elementType
	 * 		the vector elementType
	 * @param contents
	 * 		the vector contents
	 * @param adjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 */
	public ComplexVectorStructImpl(final IntegerStruct size, final LispStruct elementType,
	                               final List<LispStruct> contents, final BooleanStruct adjustable,
	                               final IntegerStruct fillPointer) {
		super(elementType, size);
		this.fillPointer = fillPointer;
		this.adjustable = adjustable;
		this.contents = new ArrayList<>(contents);
	}

	/**
	 * Public constructor, initializing the size, element-type, displacedTo, displacedIndexOffset, adjustable, and
	 * fillPointer.
	 *
	 * @param size
	 * 		the vector size
	 * @param elementType
	 * 		the vector elementType
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param adjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 */
	public ComplexVectorStructImpl(final IntegerStruct size, final LispStruct elementType,
	                               final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                               final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		super(elementType, size);
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
			throw new TypeErrorException("VECTOR has no fill-pointer to retrieve.");
		}
		return fillPointer;
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		if (fillPointer.isLessThan(IntegerStruct.ZERO) || fillPointer.isGreaterThan(totalSize)) {
			throw new ErrorException(
					"Fill-pointer " + fillPointer + " value is out of bounds for VECTOR with size " + totalSize + '.');
		}

		this.fillPointer = fillPointer;
		return fillPointer;
	}

	@Override
	public LispStruct vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a VECTOR with no fill-pointer.");
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
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (fillPointer.isGreaterThanOrEqualTo(totalSize)) {
			return NILStruct.INSTANCE;
		}

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.add(IntegerStruct.ONE);

		if (displacedTo == null) {
			contents.set(formerFillPointer.toJavaInt(), newElement);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return formerFillPointer;
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (fillPointer.isGreaterThanOrEqualTo(totalSize)) {
			if (!adjustable.toJavaPBoolean()) {
				throw new TypeErrorException("VECTOR would be extended and is not adjustable.");
			}

			final int totalSizeInt = totalSize.toJavaInt();
			final int realExtension = Math.max(extension.toJavaInt(), totalSizeInt);
			final int newTotalSize = totalSizeInt + realExtension;

			if (displacedTo == null) {
				contents.ensureCapacity(newTotalSize);
			} else {
				final List<LispStruct> displacedContents = getDisplacedToContents();
				contents = new ArrayList<>(totalSizeInt);
				contents.addAll(displacedContents);

				for (int index = displacedContents.size(); index < totalSizeInt; index++) {
					contents.set(index, NILStruct.INSTANCE);
				}

				displacedTo = null;
				displacedIndexOffset = null;
			}
			totalSize = IntegerStruct.toLispInteger(newTotalSize);
		}

		final IntegerStruct formerFillPointer = fillPointer;
		fillPointer = (IntegerStruct) formerFillPointer.add(IntegerStruct.ONE);

		if (displacedTo == null) {
			contents.set(formerFillPointer.toJavaInt(), newElement);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(formerFillPointer);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
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
	public LispStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		if (displacedTo == null) {
			return contents.get(rowMajorIndex.toJavaInt());
		}

		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndex);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		if (displacedTo == null) {
			contents.set(rowMajorIndex.toJavaInt(), newElement);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndex);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return BooleanStruct.toLispBoolean(fillPointer != null);
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, displacedIndexOffset);
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		if (displacedTo == null) {
			return contents.get(validIndex.toJavaInt());
		}

		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(validIndex);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		if (displacedTo == null) {
			contents.set(validIndex.toJavaInt(), newElement);
		} else {
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(validIndex);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
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
	public VectorStruct reverse() {
		final List<LispStruct> reversedContents;
		if (displacedTo != null) {
			reversedContents = getDisplacedToContents();
		} else if (fillPointer == null) {
			reversedContents = new ArrayList<>(contents);
		} else {
			final List<LispStruct> contentsToReverse = contents.subList(0, fillPointer.toJavaInt());
			reversedContents = new ArrayList<>(contentsToReverse);
		}
		Collections.reverse(reversedContents);
		return new ComplexVectorStructImpl(totalSize, elementType, reversedContents, adjustable, fillPointer);
	}

	@Override
	public VectorStruct nReverse() {
		if (displacedTo != null) {
			final List<LispStruct> contentsToReverse = getDisplacedToContents();
			Collections.reverse(contentsToReverse);

			for (int index = 0; index < contentsToReverse.size(); index++) {
				final FixnumStruct indexStruct = IntegerStruct.toLispInteger(index);
				final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(indexStruct);
				final LispStruct content = contentsToReverse.get(index);
				displacedTo.setfRowMajorAref(content, indexToSet);
			}
		} else if (fillPointer == null) {
			Collections.reverse(contents);
		} else {
			final List<LispStruct> contentsToReverse = contents.subList(0, fillPointer.toJavaInt());
			Collections.reverse(contentsToReverse);
		}
		return this;
	}

	/**
	 * Returns this displaced contents value as a {@link List}.
	 *
	 * @return a {@link List} representation of the displaced contents
	 */
	private List<LispStruct> getDisplacedToContents() {
		final int size = (fillPointer == null) ? totalSize.toJavaInt() : fillPointer.toJavaInt();

		final List<LispStruct> displacedToContents = new ArrayList<>();
		for (int index = 0; index < size; index++) {
			final FixnumStruct indexStruct = IntegerStruct.toLispInteger(index);
			final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(indexStruct);

			final LispStruct current = displacedTo.rowMajorAref(indexToGet);
			displacedToContents.add(current);
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
				                                Spliterator.SUBSIZED
		);
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for ComplexVectorStructImpl objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link #contents}, ensuring that each content value is treated as being 'quoted'</li>
	 * <li>Constructing a new ComplexVectorStructImpl with the built content {@link List}</li>
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
		final int elementTypeStore = generateElementType(generatorState);

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
		mv.visitVarInsn(Opcodes.ALOAD, elementTypeStore);

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
			                   GenerationConstants.VECTOR_STRUCT_NAME,
			                   GenerationConstants.VECTOR_STRUCT_TO_VECTOR_METHOD_NAME,
			                   GenerationConstants.VECTOR_STRUCT_TO_COMPLEX_VECTOR_CONTENTS_METHOD_DESC,
			                   true);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.VECTOR_STRUCT_NAME,
			                   GenerationConstants.VECTOR_STRUCT_TO_VECTOR_METHOD_NAME,
			                   GenerationConstants.VECTOR_STRUCT_TO_COMPLEX_VECTOR_DISPLACED_METHOD_DESC,
			                   true);
		}
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().toJavaPBoolean();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().toJavaPBoolean();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append("#(");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer.toJavaInt();

			for (int i = 0; i < amountToPrint; i++) {
				final LispStruct lispStruct = contents.get(i);
				final String printedLispStruct = lispStruct.toString();

				stringBuilder.append(printedLispStruct);

				if (i < (amountToPrint - 1)) {
					stringBuilder.append(' ');
				}
			}

			stringBuilder.append(')');
		} else {
			stringBuilder.append("#<");
			stringBuilder.append(typeOf());
			stringBuilder.append(' ');

			stringBuilder.append(arrayTotalSize());

			stringBuilder.append(" type ");

			stringBuilder.append(arrayElementType());

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
