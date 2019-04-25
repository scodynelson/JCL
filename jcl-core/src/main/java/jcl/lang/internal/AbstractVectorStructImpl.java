package jcl.lang.internal;

import java.util.Collections;
import java.util.List;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.TStruct;
import jcl.lang.VectorStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.statics.CommonLispSymbols;

public abstract class AbstractVectorStructImpl extends AbstractArrayStructImpl implements VectorStruct {

	protected Integer totalSize;

	protected AbstractVectorStructImpl(final LispStruct elementType, final Integer totalSize) {
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
		return IntegerStruct.toLispInteger(totalSize);
	}

	@Override
	public ListStruct arrayDimensions() {
		final IntegerStruct size = IntegerStruct.toLispInteger(totalSize);
		return ListStruct.toLispList(size);
	}

	@Override
	public boolean arrayInBoundsP(final IntegerStruct... subscripts) {
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		try {
			validateSubscript(subscript);
			return true;
		} catch (final ErrorException ignored) {
			return false;
		}
	}

	@Override
	public IntegerStruct arrayRank() {
		return IntegerStruct.ONE;
	}

	@Override
	public IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts) {
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		final int rowMajorIndex = validateSubscript(subscript);
		return IntegerStruct.toLispInteger(rowMajorIndex);
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		return IntegerStruct.toLispInteger(totalSize);
	}

	protected IntegerStruct rowMajorIndexInternal(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;
		if (numberOfSubscripts != 1) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank 1.");
		}

		return subscripts[0];
	}

	protected int validateSubscript(final IntegerStruct subscript) {
		final int subscriptInt = subscript.toJavaInt();
		if ((subscriptInt < 0) || (subscriptInt >= totalSize)) {
			throw new ErrorException("Subscript " + subscript + " is out of bounds for " + this + '.');
		}
		return subscriptInt;
	}

// =================

	@Override
	public List<Integer> getDimensions() {
		return Collections.singletonList(totalSize);
	}

// =================

	/*
	LISP-STRUCT
	 */

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
		// TODO: check this
		return BooleanStruct.toLispBoolean(
				((ArrayStruct) this).typep(typeSpecifier).toJavaPBoolean()
						|| ((SequenceStruct) this).typep(typeSpecifier).toJavaPBoolean()
		);
	}
}
