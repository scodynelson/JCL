package jcl.structs.arrays;

import jcl.structs.LispStruct;
import jcl.structs.sequences.SequenceStruct;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.arrays.Vector;

import java.util.Collections;
import java.util.List;

public class VectorStruct<TYPE extends LispStruct> extends ArrayStruct<TYPE> implements SequenceStruct {

	protected VectorStruct(final int size, final List<TYPE> contents, final LispType elementType,
						   final boolean isAdjustable, final Integer fillPointer) {
		super(Collections.singletonList(size), contents, elementType, isAdjustable, fillPointer);
	}

	@Override
	public LispType getType() {
		return Vector.INSTANCE;
	}

	// BUILDERS

	public static <TYPE extends LispStruct> VectorStruct<TYPE> getStruct(final List<TYPE> contents) {
		return new VectorStruct<>(contents.size(), contents, T.INSTANCE, false, null);
	}
}
