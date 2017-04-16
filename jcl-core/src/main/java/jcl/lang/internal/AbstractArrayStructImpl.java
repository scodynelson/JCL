package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.type.ArrayType;
import jcl.type.LispType;

public abstract class AbstractArrayStructImpl extends BuiltInClassStruct implements ArrayStruct {

	protected LispType elementType;

	protected AbstractArrayStructImpl(final ArrayType type, final LispType elementType) {
		super(type, null, null);
		this.elementType = elementType;
	}

	@Override
	public LispType arrayElementType() {
		return elementType;
	}
}
