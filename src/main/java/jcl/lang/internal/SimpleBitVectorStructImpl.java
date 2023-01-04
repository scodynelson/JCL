package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link SimpleBitVectorStructImpl} is the object representation of a Lisp 'simple-bit-vector' type.
 */
public final class SimpleBitVectorStructImpl extends AbstractBitVectorStructImpl {

	/**
	 * The contents of the vector.
	 */
	private final List<FixnumStruct> contents;

	/**
	 * Public constructor, initializing the size and contents.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param contents
	 * 		the bit-vector contents
	 */
	public SimpleBitVectorStructImpl(final IntegerStruct size, final List<FixnumStruct> contents) {
		super(size);
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
		throw new TypeErrorException("SIMPLE-BIT-VECTOR has no fill-pointer to retrieve.");
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		throw new TypeErrorException("Cannot set fill-pointer for SIMPLE-BIT-VECTOR.");
	}

	@Override
	public LispStruct vectorPop() {
		throw new TypeErrorException("Cannot pop from a SIMPLE-BIT-VECTOR with no fill-pointer.");
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) {
		throw new TypeErrorException("Cannot push into a SIMPLE-BIT-VECTOR with no fill-pointer.");
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension) {
		throw new TypeErrorException("Cannot push or extend a SIMPLE-BIT-VECTOR with no fill-pointer.");
	}

	/*
	BIT-ARRAY-STRUCT
	 */

	@Override
	public FixnumStruct bit(final IntegerStruct... subscripts) {
		return aref(subscripts);
	}

	@Override
	public FixnumStruct setfBit(final FixnumStruct newBit, final IntegerStruct... subscripts) {
		return setfAref(newBit, subscripts);
	}

	@Override
	public FixnumStruct sbit(final IntegerStruct... subscripts) {
		return aref(subscripts);
	}

	@Override
	public FixnumStruct setfSbit(final FixnumStruct newBit, final IntegerStruct... subscripts) {
		return setfAref(newBit, subscripts);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public FixnumStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		return contents.get(rowMajorIndex.toJavaInt());
	}

	@Override
	public FixnumStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final FixnumStruct newBitValue = getBit(newElement);
		final IntegerStruct rowMajorIndex = arrayRowMajorIndex(subscripts);
		contents.set(rowMajorIndex.toJavaInt(), newBitValue);
		return newBitValue;
	}

	@Override
	public FixnumStruct rowMajorAref(final IntegerStruct index) {
		final IntegerStruct validIndex = validateSubscript(index);
		return contents.get(validIndex.toJavaInt());
	}

	@Override
	public FixnumStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final FixnumStruct newBitValue = getBit(newElement);
		final IntegerStruct validIndex = validateSubscript(index);
		contents.set(validIndex.toJavaInt(), newBitValue);
		return newBitValue;
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public BitVectorStruct reverse() {
		final List<FixnumStruct> reversedContents = new ArrayList<>(contents);
		Collections.reverse(reversedContents);
		return new SimpleBitVectorStructImpl(totalSize, reversedContents);
	}

	@Override
	public BitVectorStruct nReverse() {
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
	 * Generation method for SimpleBitVectorStructImpl objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link #contents}, ensuring that each content {@link FixnumStruct} value is generated properly</li>
	 * <li>Constructing a new SimpleBitVectorStructImpl with the built content {@link List}</li>
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
		final int contentsStore = generateContents(generatorState, contents);

		mv.visitVarInsn(Opcodes.ALOAD, sizeStore);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.BIT_VECTOR_STRUCT_NAME,
		                   GenerationConstants.BIT_VECTOR_STRUCT_TO_BIT_VECTOR_METHOD_NAME,
		                   GenerationConstants.BIT_VECTOR_STRUCT_TO_SIMPLE_BIT_VECTOR_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return ListStruct.toLispList(CommonLispSymbols.SIMPLE_BIT_VECTOR, totalSize);
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.SIMPLE_BIT_VECTOR;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.SIMPLE_BIT_VECTOR) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SIMPLE_BIT_VECTOR) {
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
		final boolean printArray = CommonLispSymbols.PRINT_ARRAY_VAR.getVariableValue().toJavaPBoolean();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray) {
			stringBuilder.append("#*");

			for (final IntegerStruct integerStruct : contents) {
				final String printedIntegerStruct = integerStruct.toString();

				stringBuilder.append(printedIntegerStruct);
			}
		} else {
			stringBuilder.append("#<");
			stringBuilder.append(typeOf());
			stringBuilder.append(' ');

			stringBuilder.append(arrayTotalSize());

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
