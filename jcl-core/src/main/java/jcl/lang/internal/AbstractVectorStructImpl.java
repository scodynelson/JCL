package jcl.lang.internal;

import java.util.Collections;
import java.util.List;

import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.type.LispType;
import jcl.type.VectorType;

public abstract class AbstractVectorStructImpl extends AbstractArrayStructImpl implements VectorStruct {

	protected Integer totalSize;

	protected AbstractVectorStructImpl(final VectorType type, final LispType elementType, final Integer totalSize) {
		super(type, elementType);
		this.totalSize = totalSize;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		if (!IntegerStruct.ZERO.eq(axisNumber)) {
			throw new ErrorException("Axis " + axisNumber + " is out of bounds for " + this + '.');
		}
		return IntegerStructImpl.valueOf(totalSize);
	}

	@Override
	public ListStruct arrayDimensions() {
		final IntegerStruct size = IntegerStructImpl.valueOf(totalSize);
		return LispStructFactory.toProperList(size);
	}

	@Override
	public BooleanStruct arrayInBoundsP(final IntegerStruct... subscripts) {
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		try {
			validateSubscript(subscript);
			return TStruct.INSTANCE;
		} catch (final ErrorException ignored) {
			return NILStruct.INSTANCE;
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
		return IntegerStructImpl.valueOf(rowMajorIndex);
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		return IntegerStructImpl.valueOf(totalSize);
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
		final int subscriptInt = subscript.intValue();
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
}
