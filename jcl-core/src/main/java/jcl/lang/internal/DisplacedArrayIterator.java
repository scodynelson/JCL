package jcl.lang.internal;

import java.util.Iterator;
import java.util.NoSuchElementException;

import jcl.lang.ArrayStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;

/**
 * Iterator for array structures with displaced contents.
 */
class DisplacedArrayIterator implements Iterator<LispStruct> {

	/**
	 * The total size of the contents of the original array structure.
	 */
	private final IntegerStruct totalSize;

	/**
	 * The {@link ArrayStruct} the original array is displaced to.
	 */
	private final ArrayStruct displacedTo;

	/**
	 * The offset value into the displaced array structure where the original array content starts.
	 */
	private final IntegerStruct displacedIndexOffset;

	/**
	 * The current index of the iteration.
	 */
	private IntegerStruct current = IntegerStruct.ZERO;

	/**
	 * Constructor for building the iterator.
	 *
	 * @param totalSize
	 * 		the total size of the contents of the original array structure
	 * @param displacedTo
	 * 		the {@link ArrayStruct} the original array is displaced to
	 * @param displacedIndexOffset
	 * 		the offset value into the displaced array structure where the original array content starts
	 */
	DisplacedArrayIterator(final IntegerStruct totalSize, final ArrayStruct displacedTo,
	                       final IntegerStruct displacedIndexOffset) {
		this.totalSize = totalSize;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	@Override
	public boolean hasNext() {
		return current.isLessThan(totalSize);
	}

	@Override
	public LispStruct next() {
		if (current.isLessThan(totalSize)) {
			final IntegerStruct indexToGet = (IntegerStruct) displacedIndexOffset.add(current);
			current = (IntegerStruct) current.add(IntegerStruct.ONE);
			return displacedTo.rowMajorAref(indexToGet);
		}
		throw new NoSuchElementException("All elements consumed.");
	}
}
