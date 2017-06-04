package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NILStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.type.ArrayType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;

/**
 * The {@link ArrayStructImpl} is the object representation of a Lisp 'array' type.
 */
public abstract class ArrayStructImpl extends AbstractArrayStructImpl {

	protected boolean isAdjustable;

	protected ArrayStruct displacedTo;

	protected Integer displacedIndexOffset;

	protected ArrayStructImpl(final ArrayType arrayType, final LispType elementType, final boolean isAdjustable) {
		super(arrayType, elementType);

		this.isAdjustable = isAdjustable;
		displacedTo = null;
		displacedIndexOffset = 0;
	}

	protected ArrayStructImpl(final ArrayType arrayType, final LispType elementType, final ArrayStruct displacedTo,
	                          final Integer displacedIndexOffset, final boolean isAdjustable) {
		super(arrayType, elementType);

		this.isAdjustable = isAdjustable;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	/**
	 * Gets the array type from the provided {@link #isAdjustable} value.
	 *
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return the matching array type for the provided {@link #isAdjustable} value
	 */
	protected static ArrayType getArrayType(final boolean isAdjustable) {
		return isAdjustable ? ArrayType.INSTANCE : SimpleArrayType.INSTANCE;
	}

	@Override
	public BooleanStruct adjustableArrayP() {
		return LispStructFactory.toBoolean(isAdjustable);
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStruct.toLispInteger(displacedIndexOffset));
	}
}
