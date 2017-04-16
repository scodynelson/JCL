package jcl.lang.internal.stream;

import jcl.lang.StringStruct;
import jcl.lang.internal.AbstractVectorStructImpl;
import jcl.type.CharacterType;
import jcl.type.StringType;

public abstract class AbstractStringStructImpl extends AbstractVectorStructImpl implements StringStruct {

	protected AbstractStringStructImpl(final StringType type, final CharacterType elementType,
	                                   final Integer totalSize) {
		super(type, elementType, totalSize);
	}
}
