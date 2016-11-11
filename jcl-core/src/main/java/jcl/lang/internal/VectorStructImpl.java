package jcl.lang.internal;

import java.util.Collections;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.PrinterVariables;
import jcl.type.LispType;
import jcl.type.SimpleVectorType;
import jcl.type.TType;
import jcl.type.VectorType;

/**
 * The {@link VectorStructImpl} is the object representation of a Lisp 'vector' type.
 *
 * @param <TYPE>
 * 		the type of the vector contents
 */
public class VectorStructImpl<TYPE extends LispStruct> extends ArrayStructImpl<TYPE> implements VectorStruct<TYPE> {

	protected Integer fillPointer;

	/**
	 * Public constructor.
	 *
	 * @param contents
	 * 		the vector contents
	 */
	protected VectorStructImpl(final List<TYPE> contents) {
		this(contents.size(), contents, TType.INSTANCE, false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param size
	 * 		the vector size
	 * @param contents
	 * 		the vector contents
	 * @param elementType
	 * 		the vector elementType
	 * @param isAdjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 */
	protected VectorStructImpl(final int size, final List<TYPE> contents, final LispType elementType,
	                           final boolean isAdjustable, final Integer fillPointer) {
		this(getVectorType(isAdjustable, fillPointer), size, contents, elementType, isAdjustable, fillPointer);
	}

	/**
	 * Protected constructor.
	 *
	 * @param vectorType
	 * 		the vector type
	 * @param size
	 * 		the vector size
	 * @param contents
	 * 		the vector contents
	 * @param elementType
	 * 		the vector elementType
	 * @param isAdjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 */
	protected VectorStructImpl(final VectorType vectorType,
	                           final int size, final List<TYPE> contents, final LispType elementType,
	                           final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, Collections.singletonList(size), contents, elementType, isAdjustable);

		this.fillPointer = fillPointer;
	}

	/**
	 * Gets the vector type from the provided isAdjustable and fillPointer values.
	 *
	 * @param isAdjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 *
	 * @return the matching vector type for the provided isAdjustable and fillPointer values
	 */
	private static VectorType getVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? VectorType.INSTANCE : SimpleVectorType.INSTANCE;
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final List<T> contents) {
		return new VectorStructImpl<>(contents);
	}

	/**
	 * Getter for vector {@link #fillPointer} property.
	 *
	 * @return vector {@link #fillPointer} property
	 */
	@Override
	public Integer getFillPointer() {
		return fillPointer;
	}

	/**
	 * Setter for vector {@link #fillPointer} property.
	 *
	 * @param fillPointer
	 * 		new vector {@link #fillPointer} property value
	 */
	@Override
	public void setFillPointer(final Integer fillPointer) {
		this.fillPointer = fillPointer;
	}

	/**
	 * Pops the element at the {@link #fillPointer} index and decreases the {@link #fillPointer} by 1.
	 *
	 * @return the element popped from the {@link #fillPointer} index
	 *
	 * @throws ErrorException
	 * 		if the vector has no {@link #fillPointer} or the {@link #fillPointer} is 0
	 */
	@Override
	public TYPE pop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a vector that has no fill-pointer.");
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
	 * Pushes the provided {@code element} into the current {@link #fillPointer} index.
	 *
	 * @param element
	 * 		the element to push into the vector
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no {@link #fillPointer}
	 */
	@Override
	public int push(final TYPE element) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a vector that has no fill-pointer.");
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
	 * Pushes the provided {@code element} into the current {@link #fillPointer} index and extends the vector to the
	 * current size of the contents plus the provided {@code extensionAmount}.
	 *
	 * @param element
	 * 		the element to push into the vector
	 * @param extensionAmount
	 * 		the amount to extend the vector when pushing
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no {@link #fillPointer} or the vector is not adjustable
	 */
	@Override
	public int pushExtend(final TYPE element, final int extensionAmount) {
		if (!isAdjustable) {
			throw new TypeErrorException("Vector is not an adjustable array.");
		}
		return push(element);
	}

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append("#(");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

			for (int i = 0; i < amountToPrint; i++) {
				final TYPE lispStruct = contents.get(i);
				final String printedLispStruct = lispStruct.toString();

				stringBuilder.append(printedLispStruct);

				if (i < (amountToPrint - 1)) {
					stringBuilder.append(' ');
				}
			}

			stringBuilder.append(')');
		} else {
			final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			stringBuilder.append(totalSize);

			stringBuilder.append(" type ");

			final String elementTypeClassName = elementType.getClass().getName().toUpperCase();
			stringBuilder.append(elementTypeClassName);

			if (fillPointer != null) {
				stringBuilder.append(" fill-pointer ");
				stringBuilder.append(fillPointer);
			}

			if (isAdjustable) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
