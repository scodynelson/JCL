package jcl.lang.internal.array;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.SimpleVectorStruct;
import jcl.type.LispType;
import jcl.type.VectorType;

public class SimpleVectorStructImpl<TYPE extends LispStruct> extends VectorStructImpl<TYPE> implements SimpleVectorStruct<TYPE> {

	protected SimpleVectorStructImpl(final List<TYPE> contents) {
		super(contents);
	}

	protected SimpleVectorStructImpl(final int size, final List<TYPE> contents, final LispType elementType, final boolean isAdjustable, final Integer fillPointer) {
		super(size, contents, elementType, isAdjustable, fillPointer);
	}

	protected SimpleVectorStructImpl(final VectorType vectorType, final int size, final List<TYPE> contents, final LispType elementType, final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, size, contents, elementType, isAdjustable, fillPointer);
	}
}
