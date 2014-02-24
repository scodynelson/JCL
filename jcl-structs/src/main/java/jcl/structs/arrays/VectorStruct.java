package jcl.structs.arrays;

import jcl.structs.LispStruct;
import jcl.structs.sequences.SequenceStruct;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.arrays.Vector;

import java.util.Collections;
import java.util.List;

public class VectorStruct<TYPE extends LispStruct> extends ArrayStruct<TYPE> implements SequenceStruct {

	public VectorStruct(final List<TYPE> contents) {
		this(contents.size(), contents, T.INSTANCE, false, null);
	}

	public VectorStruct(final int size, final List<TYPE> contents, final LispType elementType,
						final boolean isAdjustable, final Integer fillPointer) {
		this(Vector.INSTANCE, size, contents, elementType, isAdjustable, fillPointer);
	}

	protected VectorStruct(final Vector vectorType,
						   final int size, final List<TYPE> contents, final LispType elementType,
						   final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, Collections.singletonList(size), contents, elementType, isAdjustable, fillPointer);
	}
}
