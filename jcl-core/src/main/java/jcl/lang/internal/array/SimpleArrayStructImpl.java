package jcl.lang.internal.array;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.SimpleArrayStruct;
import jcl.type.ArrayType;
import jcl.type.LispType;

public class SimpleArrayStructImpl<TYPE extends LispStruct> extends ArrayStructImpl<TYPE> implements SimpleArrayStruct<TYPE> {

	protected SimpleArrayStructImpl(final List<Integer> dimensions, final List<TYPE> contents) {
		super(dimensions, contents);
	}

	protected SimpleArrayStructImpl(final List<Integer> dimensions, final List<TYPE> contents, final LispType elementType, final boolean isAdjustable) {
		super(dimensions, contents, elementType, isAdjustable);
	}

	protected SimpleArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final List<TYPE> contents, final LispType elementType, final boolean isAdjustable) {
		super(arrayType, dimensions, contents, elementType, isAdjustable);
	}
}
