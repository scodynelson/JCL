package jcl.lang.internal;

import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.ArrayDisplacement;
import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.statics.PrinterVariables;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link ComplexArrayStructImpl} is the object representation of a Lisp 'array' type.
 */
public class ComplexArrayStructImpl extends AbstractMultiDimensionArrayStructImpl {

	/**
	 * The contents of the array.
	 */
	private List<LispStruct> contents;

	/**
	 * Whether or not the array is adjustable.
	 */
	private BooleanStruct adjustable;

	/**
	 * The {@link ArrayStruct} structure that this array is displaced to. If {@code null}, this structure is not
	 * displaced to anything.
	 */
	private ArrayStruct displacedTo;

	/**
	 * The index offset into the {@link #displacedTo} structure when looking for or updating elements.
	 */
	private IntegerStruct displacedIndexOffset;

	/**
	 * Public constructor, initializing the dimensions, element-type, contents, and adjustable.
	 *
	 * @param dimensions
	 * 		the array size
	 * @param contents
	 * 		the vector contents
	 * @param elementType
	 * 		the array elementType
	 * @param adjustable
	 * 		whether or not the array is adjustable
	 */
	public ComplexArrayStructImpl(final List<IntegerStruct> dimensions, final LispStruct elementType,
	                              final List<LispStruct> contents, final BooleanStruct adjustable) {
		super(dimensions, elementType);
		this.adjustable = adjustable;
		this.contents = contents;
	}

	/**
	 * Public constructor, initializing the size, element-type, displacedTo, displacedIndexOffset, and adjustable.
	 *
	 * @param dimensions
	 * 		the array size
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param elementType
	 * 		the array elementType
	 * @param adjustable
	 * 		whether or not the array is adjustable
	 */
	public ComplexArrayStructImpl(final List<IntegerStruct> dimensions, final LispStruct elementType,
	                              final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                              final BooleanStruct adjustable) {
		super(dimensions, elementType);
		this.adjustable = adjustable;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
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
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		if (displacedTo == null) {
			return contents.get(rowMajorIndex);
		}

		final FixnumStruct rowMajorIndexStruct = IntegerStruct.toLispInteger(rowMajorIndex);
		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndexStruct);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		if (displacedTo == null) {
			contents.set(rowMajorIndex, newElement);
		} else {
			final FixnumStruct rowMajorIndexStruct = IntegerStruct.toLispInteger(rowMajorIndex);
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(rowMajorIndexStruct);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	@Override
	public ArrayDisplacement arrayDisplacement() {
		return new ArrayDisplacement(displacedTo, displacedIndexOffset);
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final int validIndex = validateSubscript(index);
		if (displacedTo == null) {
			return contents.get(validIndex);
		}

		final FixnumStruct validIndexStruct = IntegerStruct.toLispInteger(validIndex);
		final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(validIndexStruct);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final int validIndex = validateSubscript(index);
		if (displacedTo == null) {
			contents.set(validIndex, newElement);
		} else {
			final FixnumStruct validIndexStruct = IntegerStruct.toLispInteger(validIndex);
			final IntegerStruct indexToSet = (IntegerStruct) displacedIndexOffset.add(validIndexStruct);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ArrayStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link ComplexArrayStructImpl#dimensions} values</li>
	 * <li>Building the {@link ComplexArrayStructImpl#contents} values, ensuring that each content value is treated as being
	 * 'quoted'</li>
	 * <li>Constructing a new {@link ArrayStruct} with the built dimension and content {@link List}s</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int dimensionsStore = generateDimensions(generatorState);
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

		mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
		mv.visitVarInsn(Opcodes.ALOAD, elementTypeStore);

		if (displacedTo == null) {
			mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		} else {
			mv.visitVarInsn(Opcodes.ALOAD, displacedToStore);
			mv.visitVarInsn(Opcodes.ALOAD, displacedIndexOffsetStore);
		}

		mv.visitVarInsn(Opcodes.ALOAD, adjustableStore);

		if (displacedTo == null) {
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.ARRAY_STRUCT_NAME,
			                   GenerationConstants.ARRAY_STRUCT_TO_ARRAY_METHOD_NAME,
			                   GenerationConstants.ARRAY_STRUCT_TO_COMPLEX_ARRAY_CONTENTS_METHOD_DESC,
			                   true);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.ARRAY_STRUCT_NAME,
			                   GenerationConstants.ARRAY_STRUCT_TO_ARRAY_METHOD_NAME,
			                   GenerationConstants.ARRAY_STRUCT_TO_COMPLEX_ARRAY_DISPLACED_METHOD_DESC,
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

		final int rank = multidimensionalCounter.getDimension();
		if (printArray || printReadably) {
			stringBuilder.append('#');

			stringBuilder.append(rank);
			stringBuilder.append('A');
			if (rank > 0) {
				stringBuilder.append('(');
			}

			final int contentsSize = contents.size();
			for (int i = 0; i < contentsSize; i++) {
				final LispStruct lispStruct = contents.get(i);
				final String printedLispStruct = lispStruct.toString();

				stringBuilder.append(printedLispStruct);

				if (i < (contentsSize - 1)) {
					stringBuilder.append(' ');
				}
			}

			if (rank > 0) {
				stringBuilder.append(')');
			}
		} else {
			stringBuilder.append("#<");
			stringBuilder.append(typeOf());
			stringBuilder.append(' ');

			for (int i = 0; i < rank; i++) {
				stringBuilder.append(dimensions.get(i));

				if ((i + 1) != rank) {
					stringBuilder.append('x');
				}
			}

			stringBuilder.append(" type ");
			stringBuilder.append(elementType);

			if (adjustable.toJavaPBoolean()) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
