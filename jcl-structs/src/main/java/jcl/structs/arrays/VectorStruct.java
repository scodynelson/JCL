package jcl.structs.arrays;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.structs.sequences.SequenceStruct;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.arrays.SimpleVector;
import jcl.types.arrays.Vector;

import java.util.Collections;
import java.util.List;

/**
 * The {@code VectorStruct} is the object representation of a Lisp 'vector' type.
 *
 * @param <TYPE> the type of the vector contents
 */
public class VectorStruct<TYPE extends LispStruct> extends ArrayStruct<TYPE> implements SequenceStruct {

	protected Integer fillPointer;

	/**
	 * Public constructor.
	 *
	 * @param contents the vector contents
	 * @throws TypeErrorException   if any of the provided {@code contents} are not the same type as the provided {@code elementType}
	 * @throws SimpleErrorException if the provided {@code contents} do not match the provided {@code dimensions}
	 */
	public VectorStruct(final List<TYPE> contents) throws TypeErrorException, SimpleErrorException {
		this(contents.size(), contents, T.INSTANCE, false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param size         the vector size
	 * @param contents     the vector contents
	 * @param elementType  the vector elementType
	 * @param isAdjustable whether or not the vector is adjustable
	 * @param fillPointer  the vector fillPointer
	 * @throws TypeErrorException   if any of the provided {@code contents} are not the same type as the provided {@code elementType}
	 * @throws SimpleErrorException if the provided {@code contents} do not match the provided {@code dimensions}
	 */
	public VectorStruct(final int size, final List<TYPE> contents, final LispType elementType,
						final boolean isAdjustable, final Integer fillPointer) throws TypeErrorException, SimpleErrorException {
		this(getVectorType(isAdjustable, fillPointer), size, contents, elementType, isAdjustable, fillPointer);
	}

	/**
	 * Protected constructor.
	 *
	 * @param vectorType   the vector type
	 * @param size         the vector size
	 * @param contents     the vector contents
	 * @param elementType  the vector elementType
	 * @param isAdjustable whether or not the vector is adjustable
	 * @param fillPointer  the vector fillPointer
	 * @throws TypeErrorException   if any of the provided {@code contents} are not the same type as the provided {@code elementType}
	 * @throws SimpleErrorException if the provided {@code contents} do not match the provided {@code dimensions}
	 */
	protected VectorStruct(final Vector vectorType,
						   final int size, final List<TYPE> contents, final LispType elementType,
						   final boolean isAdjustable, final Integer fillPointer) throws TypeErrorException, SimpleErrorException {
		super(vectorType, Collections.singletonList(size), contents, elementType, isAdjustable);

		this.fillPointer = fillPointer;
	}

	/**
	 * This method gets the vector type from the provided isAdjustable and fillPointer values.
	 *
	 * @param isAdjustable whether or not the vector is adjustable
	 * @param fillPointer  the vector fillPointer
	 * @return the matching vector type for the provided isAdjustable and fillPointer values
	 */
	private static Vector getVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? Vector.INSTANCE : SimpleVector.INSTANCE;
	}

	/**
	 * Getter for vector fillPointer property.
	 *
	 * @return vector fillPointer property
	 */
	public Integer getFillPointer() {
		return fillPointer;
	}

	/**
	 * Setter for vector fillPointer property.
	 *
	 * @param fillPointer new vector fillPointer property value
	 */
	public void setFillPointer(final Integer fillPointer) {
		this.fillPointer = fillPointer;
	}

	/**
	 * This method pops the element at the fillPointer index and decreases the fillPointer by 1.
	 *
	 * @return the element popped from the fillPointer index
	 * @throws ErrorException if the vector has no fill-pointer or the fill-pointer is 0
	 */
	public TYPE pop() throws ErrorException {
		if (fillPointer == null) {
			throw new TypeErrorException("Vector has no fill-pointer.");
		}
		if (fillPointer == 0) {
			throw new ErrorException("Fill pointer is 0.");
		}
		fillPointer--;

		final TYPE element = contents.get(fillPointer);

		contents.remove(fillPointer.intValue());
		return element;
	}

	/**
	 * This method pushes the provided {@code element} into the current fillPointer index.
	 *
	 * @param element the element to push into the vector
	 * @return the location of the newly added element
	 * @throws TypeErrorException if the vector has no fill-pointer
	 */
	public int push(final TYPE element) throws TypeErrorException {
		if (fillPointer == null) {
			throw new TypeErrorException("Vector has no fill-pointer.");
		}
		fillPointer++;

		if (fillPointer >= contents.size()) {
			contents.add(fillPointer, element);
		} else {
			contents.set(fillPointer, element);
		}
		return fillPointer;
	}

	/**
	 * This method pushes the provided {@code element} into the current fillPointer index and extends the vector to the
	 * current size of the contents plus the provided {@code extensionAmount}.
	 *
	 * @param element         the element to push into the vector
	 * @param extensionAmount the amount to extend the vector when pushing
	 * @return the location of the newly added element
	 * @throws TypeErrorException if the vector has no fill-pointer or the vector is not adjustable
	 */
	public int pushExtend(final TYPE element, final int extensionAmount) throws TypeErrorException {
		if (!isAdjustable) {
			throw new TypeErrorException("Vector is not an adjustable array.");
		}
		return push(element);
	}

	@Override
	public String toString() {
		return "VectorStruct{"
				+ "contents=" + contents
				+ ", rank=" + rank
				+ ", dimensions=" + dimensions
				+ ", elementType=" + elementType
				+ ", isAdjustable=" + isAdjustable
				+ "fillPointer=" + fillPointer
				+ '}';
	}
}
