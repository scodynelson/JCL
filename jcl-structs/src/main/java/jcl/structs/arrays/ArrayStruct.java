package jcl.structs.arrays;

import jcl.structs.LispStruct;
import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.arrays.Array;

import java.util.List;

public class ArrayStruct<TYPE extends LispStruct> extends BuiltInClassStruct {

	private final List<TYPE> contents;
	private final int rank;
	private final List<Integer> dimensions;
	private final LispType elementType;
	private final boolean isAdjustable;
	private final Integer fillPointer;

	public ArrayStruct(final List<Integer> dimensions, final List<TYPE> contents) {
		this(Array.INSTANCE, dimensions, contents, T.INSTANCE, false, null);
	}

	protected ArrayStruct(final Array arrayType,
						  final List<Integer> dimensions, final List<TYPE> contents, final LispType elementType,
						  final boolean isAdjustable, final Integer fillPointer) {
		super(Array.INSTANCE, null, null);

		this.contents = contents;

		// TODO: We really should do a check in here to verify the dimensions are the same as the contents
		this.dimensions = dimensions;
		rank = dimensions.size();

		// TODO: We really should do a check in here to check each element against the element type
		this.elementType = elementType;
		this.isAdjustable = isAdjustable;

		if ((rank != 1) && (fillPointer != null)) {
			throw new IllegalArgumentException("Fill pointer supplied for non-singular dimensional array: " + fillPointer);
		}
		this.fillPointer = fillPointer;
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
}
