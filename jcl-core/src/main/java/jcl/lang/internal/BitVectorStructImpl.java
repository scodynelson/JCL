package jcl.lang.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.BitVectorStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.PrinterVariables;
import jcl.type.BitType;
import jcl.type.BitVectorType;
import jcl.type.SimpleBitVectorType;

/**
 * The {@link BitVectorStructImpl} is the object representation of a Lisp 'bit-vector' type.
 */
public final class BitVectorStructImpl extends VectorStructImpl<IntegerStruct> implements BitVectorStruct {

	private static final Pattern BIT_PATTERN = Pattern.compile("[0|1]+");

	private BitVectorStructImpl(final BitVectorType bitVectorType, final Integer size, final List<IntegerStruct> contents,
	                            final boolean isAdjustable, final Integer fillPointer) {
		super(bitVectorType, size, BitType.INSTANCE, contents, isAdjustable, fillPointer);
	}

	public static BitVectorStruct valueOf(final Integer size, final IntegerStruct initialElement,
	                                      final boolean isAdjustable, final Integer fillPointer) {
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(size)
		                                                  .collect(Collectors.toList());
		final BitVectorType bitVectorType = getBitVectorType(isAdjustable, fillPointer);
		return new BitVectorStructImpl(bitVectorType, size, initialContents, isAdjustable, fillPointer);
	}

	public static BitVectorStruct valueOf(final Integer size, final List<IntegerStruct> initialContents,
	                                      final boolean isAdjustable, final Integer fillPointer) {
		final BitVectorType bitVectorType = getBitVectorType(isAdjustable, fillPointer);
		return new BitVectorStructImpl(bitVectorType, size, initialContents, isAdjustable, fillPointer);
	}

	public static BitVectorStruct valueOf(final Integer size, final IntegerStruct initialElement) {
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(size)
		                                                  .collect(Collectors.toList());
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, size, initialContents, false, null);
	}

	public static BitVectorStruct valueOf(final Integer size, final List<IntegerStruct> initialContents) {
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, size, initialContents, false, null);
	}

	/*
		Old Builders
	 */

	public static BitVectorStruct valueOf(final String bitString) {
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, bitString.length(), getBitList(bitString), false, null);
	}

	public static BitVectorStruct valueOfCont(final List<IntegerStruct> contents) {
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, contents.size(), contents, false, null);
	}

	/**
	 * Gets the bit-vector type from the provided isAdjustable and fillPointer values.
	 *
	 * @param isAdjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 *
	 * @return the matching bit-vector type for the provided isAdjustable and fillPointer values
	 */
	private static BitVectorType getBitVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? BitVectorType.INSTANCE : SimpleBitVectorType.INSTANCE;
	}

	/**
	 * Gets a list of {@link IntegerStruct}s from the provided {@link String} value.
	 *
	 * @param bitString
	 * 		the Java string to convert to a list of {@link IntegerStruct}s
	 *
	 * @return a list of {@link IntegerStruct}s from the provided {@link String} value
	 */
	private static List<IntegerStruct> getBitList(final String bitString) {
		if (!bitString.isEmpty() && !BIT_PATTERN.matcher(bitString).matches()) {
			throw new TypeErrorException("Input contains characters not of type " + BitType.INSTANCE + ": " + bitString + '.');
		}

		final List<IntegerStruct> bitList = new ArrayList<>(bitString.length());
		for (final char character : bitString.toCharArray()) {
			if (character == '0') {
				bitList.add(IntegerStruct.ZERO);
			} else if (character == '1') {
				bitList.add(IntegerStruct.ONE);
			}
		}
		return bitList;
	}

	@Override
	public String toString() {
		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray) {
			stringBuilder.append("#*");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

			for (int i = 0; i < amountToPrint; i++) {
				final IntegerStruct integerStruct = contents.get(i);
				final String printedIntegerStruct = integerStruct.toString();

				stringBuilder.append(printedIntegerStruct);
			}
		} else {
			final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			stringBuilder.append(arrayTotalSize());

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
