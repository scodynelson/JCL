package jcl.lang.internal;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.number.IntegerStructImpl;
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

	Integer fillPointer;

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
	VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                 final List<TYPE> contents, final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, Collections.singletonList(size), elementType, contents, isAdjustable);

		this.fillPointer = fillPointer;
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final T initialElement, final boolean isAdjustable,
	                                                             final Integer fillPointer) {
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(size)
		                                      .collect(Collectors.toList());
		final VectorType vectorType = getVectorType(isAdjustable, fillPointer);
		return new VectorStructImpl<>(vectorType, size, elementType, initialContents, isAdjustable, fillPointer);
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final List<T> initialContents,
	                                                             final boolean isAdjustable,
	                                                             final Integer fillPointer) {
		final VectorType vectorType = getVectorType(isAdjustable, fillPointer);
		return new VectorStructImpl<>(vectorType, size, elementType, initialContents, isAdjustable, fillPointer);
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final T initialElement) {
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(size)
		                                      .collect(Collectors.toList());
		return new VectorStructImpl<>(SimpleVectorType.INSTANCE, size, elementType, initialContents, false, null);
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final List<T> initialContents) {
		return new VectorStructImpl<>(SimpleVectorType.INSTANCE, size, elementType, initialContents, false, null);
	}

	/*
		Old Builders
	 */

	public static <T extends LispStruct> VectorStruct<T> valueOf(final List<T> contents) {
		return new VectorStructImpl<>(SimpleVectorType.INSTANCE, contents.size(), TType.INSTANCE, contents, false,
		                              null);
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

	@Override
	public IntegerStruct fillPointer() {
		return IntegerStructImpl.valueOf(fillPointer);
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		this.fillPointer = fillPointer.intValue();
		return fillPointer;
	}

	@Override
	public TYPE vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a VECTOR with no fill-pointer.");
		}
		if (fillPointer == 0) {
			throw new ErrorException("Nothing left to pop.");
		}

		fillPointer--;
		final TYPE element = contents.get(fillPointer);
		contents.set(fillPointer, null);
		return element;
	}

	@Override
	public LispStruct vectorPush(final TYPE element) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (fillPointer >= contents.size()) {
			return NILStruct.INSTANCE;
		}

		final Integer previousFillPointer = fillPointer++;
		contents.set(fillPointer, element);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	@Override
	public IntegerStruct vectorPushExtend(final TYPE element, final IntegerStruct extensionAmount) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (!isAdjustable) {
			throw new TypeErrorException("VECTOR is not adjustable.");
		}
		if (fillPointer >= contents.size()) {
//			adjustArray(fillPointer + extensionAmount);
		}

		final Integer previousFillPointer = fillPointer++;
		contents.set(fillPointer, element);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new VectorIterator<>(contents.iterator());
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(contents.iterator(),
		                                contents.size(),
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.IMMUTABLE |
				                                Spliterator.SUBSIZED
		);
	}

	private static final class VectorIterator<TYPE extends LispStruct> implements Iterator<LispStruct> {

		private Iterator<TYPE> iterator;

		private VectorIterator(final Iterator<TYPE> iterator) {
			this.iterator = iterator;
		}

		@Override
		public boolean hasNext() {
			return iterator.hasNext();
		}

		@Override
		public LispStruct next() {
			return iterator.next();
		}

		@Override
		public void forEachRemaining(final Consumer<? super LispStruct> action) {
			iterator.forEachRemaining(action);
		}
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

			stringBuilder.append(arrayTotalSize());

			stringBuilder.append(" type ");

			final String elementTypeClassName = arrayElementType().getClass().getName().toUpperCase();
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
