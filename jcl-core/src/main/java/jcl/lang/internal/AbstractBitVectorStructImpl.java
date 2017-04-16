package jcl.lang.internal;

import jcl.lang.BitVectorStruct;
import jcl.type.BitType;
import jcl.type.BitVectorType;

public abstract class AbstractBitVectorStructImpl extends AbstractVectorStructImpl implements BitVectorStruct {

	protected AbstractBitVectorStructImpl(final BitVectorType type, final Integer totalSize) {
		super(type, BitType.INSTANCE, totalSize);
	}
}
