package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.TStruct;
import jcl.lang.VectorStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link SimpleVectorStructImpl} is the object representation of a Lisp 'simple-vector' type.
 */
public class SimpleVectorStructImpl extends AbstractVectorStructImpl {

	/**
	 * The contents of the vector.
	 */
	private final List<LispStruct> contents;

	/**
	 * Public constructor, initializing the size, element-type, and contents.
	 *
	 * @param size
	 * 		the vector size
	 * @param elementType
	 * 		the vector elementType
	 * @param contents
	 * 		the vector contents
	 */
	public SimpleVectorStructImpl(final IntegerStruct size, final LispStruct elementType,
	                              final List<LispStruct> contents) {
		super(elementType, size);
		this.contents = contents;
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public LispStruct svref(final FixnumStruct index) {
		return aref(index);
	}

	@Override
	public LispStruct setfSvref(final LispStruct newElement, final FixnumStruct index) {
		return setfAref(newElement, index);
	}

	@Override
	public IntegerStruct fillPointer() {
		throw new TypeErrorException("SIMPLE-VECTOR has no fill-pointer to retrieve.");
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		throw new TypeErrorException("Cannot set fill-pointer for SIMPLE-VECTOR.");
	}

	@Override
	public LispStruct vectorPop() {
		throw new TypeErrorException("Cannot pop from a SIMPLE-VECTOR with no fill-pointer.");
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		throw new TypeErrorException("Cannot push into a SIMPLE-VECTOR with no fill-pointer.");
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		throw new TypeErrorException("Cannot push or extend a SIMPLE-VECTOR with no fill-pointer.");
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		return contents.get(rowMajorIndex.toJavaInt());
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		contents.set(rowMajorIndex.toJavaInt(), newElement);
		return newElement;
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		return contents.get(validIndex.toJavaInt());
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		contents.set(validIndex.toJavaInt(), newElement);
		return newElement;
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public VectorStruct reverse() {
		final List<LispStruct> reversedContents = new ArrayList<>(contents);
		Collections.reverse(reversedContents);
		return new SimpleVectorStructImpl(totalSize, elementType, reversedContents);
	}

	@Override
	public VectorStruct nReverse() {
		Collections.reverse(contents);
		return this;
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new WrapperIterator<>(contents.iterator());
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return new WrapperSpliterator<>(contents.spliterator());
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for SimpleVectorStructImpl objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link #contents}, ensuring that each content value is treated as being 'quoted'</li>
	 * <li>Constructing a new SimpleVectorStructImpl with the built content {@link List}</li>
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
		final int contentsStore = generateContents(generatorState, contents);

		mv.visitVarInsn(Opcodes.ALOAD, sizeStore);
		mv.visitVarInsn(Opcodes.ALOAD, elementTypeStore);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.VECTOR_STRUCT_NAME,
		                   GenerationConstants.VECTOR_STRUCT_TO_VECTOR_METHOD_NAME,
		                   GenerationConstants.VECTOR_STRUCT_TO_SIMPLE_VECTOR_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return ListStruct.toLispList(CommonLispSymbols.SIMPLE_VECTOR, totalSize);
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.SIMPLE_VECTOR;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.SIMPLE_VECTOR) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SIMPLE_VECTOR) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = CommonLispSymbols.PRINT_ARRAY_VAR.getVariableValue().toJavaPBoolean();
		final boolean printReadably = CommonLispSymbols.PRINT_READABLY_VAR.getVariableValue().toJavaPBoolean();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append("#(");

			final int amountToPrint = contents.size();

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

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
