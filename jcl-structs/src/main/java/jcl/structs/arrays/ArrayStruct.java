package jcl.structs.arrays;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.arrays.Array;

import java.util.List;

public class ArrayStruct<TYPE extends LispStruct> implements LispStruct {

	protected final List<TYPE> contents;
	protected final int rank;
	protected final List<Integer> dimensions;
	protected final LispType elementType;
	protected final boolean isAdjustable;
	protected final Integer fillPointer;

	protected ArrayStruct(final List<Integer> dimensions, final List<TYPE> contents, final LispType elementType,
						  final boolean isAdjustable, final Integer fillPointer) {

		this.contents = contents;

		// TODO: We really should do a check in here to verify the dimensions are the same as the contents
		this.dimensions = dimensions;
		rank = dimensions.size();

		// TODO: We really should do a check in here to check each element against the element type; but how do we do this???
		this.elementType = elementType;
		this.isAdjustable = isAdjustable;

		if ((rank != 1) && (fillPointer != null)) {
			throw new IllegalArgumentException("Fill pointer supplied for non-singular dimensional array: " + fillPointer);
		}
		this.fillPointer = fillPointer;
	}

	@Override
	public LispType getType() {
		return Array.INSTANCE;
	}

	public List<TYPE> getContents() {
		return contents;
	}

	public int getRank() {
		return rank;
	}

	public List<Integer> getDimensions() {
		return dimensions;
	}

	public LispType getElementType() {
		return elementType;
	}

	public boolean isAdjustable() {
		return isAdjustable;
	}

	public Integer getFillPointer() {
		return fillPointer;
	}

	public boolean isSimple() {
		return !isAdjustable && (fillPointer == null);
	}

	@Override
	public String toString() {
		return "ArrayStruct{" +
				"contents=" + contents +
				", rank=" + rank +
				", dimensions=" + dimensions +
				", elementType=" + elementType +
				", isAdjustable=" + isAdjustable +
				", fillPointer=" + fillPointer +
				'}';
	}

	// BUILDERS

	public static <TYPE extends LispStruct> ArrayStruct<TYPE> getStruct(final List<Integer> dimensions, final List<TYPE> contents) {
		return new ArrayStruct<>(dimensions, contents, T.INSTANCE, false, null);
	}
}
