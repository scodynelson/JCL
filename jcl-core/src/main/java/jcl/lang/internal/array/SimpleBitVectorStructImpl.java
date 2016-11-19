package jcl.lang.internal.array;

import java.util.List;

import jcl.lang.IntegerStruct;
import jcl.lang.SimpleBitVectorStruct;

public class SimpleBitVectorStructImpl extends BitVectorStructImpl implements SimpleBitVectorStruct {

	private SimpleBitVectorStructImpl(final String bitString) {
		super(bitString);
	}

	private SimpleBitVectorStructImpl(final List<IntegerStruct> contents) {
		super(contents);
	}

	private SimpleBitVectorStructImpl(final int size, final List<IntegerStruct> contents, final boolean isAdjustable, final Integer fillPointer) {
		super(size, contents, isAdjustable, fillPointer);
	}
}
