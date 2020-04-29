package jcl.lang.internal;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.VectorStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Abstract base class for 'vector' types.
 */
public abstract class AbstractVectorStructImpl extends AbstractArrayStructImpl implements VectorStruct {

	/**
	 * The total size of the 'vector'.
	 */
	protected IntegerStruct totalSize;

	/**
	 * Protected constructor, initializing the element-type and the total size.
	 *
	 * @param elementType
	 * 		the element-type of the 'vector'
	 * @param totalSize
	 * 		the total size of the 'vector'
	 */
	protected AbstractVectorStructImpl(final LispStruct elementType, final IntegerStruct totalSize) {
		super(elementType);
		this.totalSize = totalSize;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		if (!IntegerStruct.ZERO.eql(axisNumber)) {
			throw new ErrorException("Axis " + axisNumber + " is out of bounds for " + this + '.');
		}
		return totalSize;
	}

	@Override
	public ListStruct arrayDimensions() {
		return ListStruct.toLispList(totalSize);
	}

	@Override
	public IntegerStruct arrayRank() {
		return IntegerStruct.ONE;
	}

	@Override
	public IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;
		if (numberOfSubscripts != 1) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank 1.");
		}

		final IntegerStruct subscript = subscripts[0];
		return validateSubscript(subscript);
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		return totalSize;
	}

	/**
	 * Validates the provided subscript is a valid subscript for retrieving a value from the 'vector', meaning the value
	 * is not less-than 0 and is not greater-than-or-equal-to the total size of the 'vector'.
	 *
	 * @param subscript
	 * 		the subscript value to validate
	 *
	 * @return the subscript value
	 *
	 * @throws ErrorException
	 * 		if the subscript value is not valid
	 */
	protected IntegerStruct validateSubscript(final IntegerStruct subscript) {
		if (subscript.isLessThan(IntegerStruct.ZERO) || subscript.isGreaterThanOrEqualTo(totalSize)) {
			throw new ErrorException("Subscript " + subscript + " is out of bounds for " + this + '.');
		}
		return subscript;
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public IntegerStruct length() {
		return totalSize;
	}

	@Override
	public LispStruct elt(final IntegerStruct index) {
		final IntegerStruct validIndex = validateIndex(index);
		return aref(validIndex);
	}

	@Override
	public LispStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		final IntegerStruct validIndex = validateIndex(index);
		return setfAref(newElement, validIndex);
	}

	/**
	 * Helper method for {@link #elt(IntegerStruct)} and {@link #setfElt(LispStruct, IntegerStruct)} for validating
	 * provided index values. This is primarily here to deal with fill-pointer values, which affect the behavior of
	 * 'elt' operations in complex string structures.
	 *
	 * @param index
	 * 		the index to validate
	 *
	 * @return the validated index
	 */
	protected IntegerStruct validateIndex(final IntegerStruct index) {
		return validateSubscript(index);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return ListStruct.toLispList(CommonLispSymbols.VECTOR, totalSize);
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.VECTOR;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.VECTOR) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.VECTOR) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.SEQUENCE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SEQUENCE) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	protected int generateSize(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int sizeStore = methodBuilder.getNextAvailableStore();
		totalSize.generate(generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, sizeStore);

		return sizeStore;
	}
}
