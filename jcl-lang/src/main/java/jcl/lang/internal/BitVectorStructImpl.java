package jcl.lang.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import jcl.lang.BitVectorStruct;
import jcl.lang.statics.PrinterVariables;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.number.IntegerStructImpl;
import jcl.type.BitType;
import jcl.type.BitVectorType;
import jcl.type.SimpleBitVectorType;

/**
 * The {@link BitVectorStructImpl} is the object representation of a Lisp 'bit-vector' type.
 */
public final class BitVectorStructImpl extends VectorStructImpl<IntegerStructImpl> implements BitVectorStruct {

	private static final Pattern BIT_PATTERN = Pattern.compile("[0|1]+");

	/**
	 * Public constructor.
	 *
	 * @param bitString
	 * 		a Java string used for the bit-vector contents
	 */
	private BitVectorStructImpl(final String bitString) {
		this(bitString.length(), getBitList(bitString), false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param contents
	 * 		the bit-vector contents
	 */
	private BitVectorStructImpl(final List<IntegerStructImpl> contents) {
		this(contents.size(), contents, false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param contents
	 * 		the bit-vector contents
	 * @param isAdjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 */
	private BitVectorStructImpl(final int size, final List<IntegerStructImpl> contents, final boolean isAdjustable, final Integer fillPointer) {
		super(getBitVectorType(isAdjustable, fillPointer), size, contents, BitType.INSTANCE, isAdjustable, fillPointer);
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
	 * Gets a list of {@link IntegerStructImpl}s from the provided {@link String} value.
	 *
	 * @param bitString
	 * 		the Java string to convert to a list of {@link IntegerStructImpl}s
	 *
	 * @return a list of {@link IntegerStructImpl}s from the provided {@link String} value
	 */
	private static List<IntegerStructImpl> getBitList(final String bitString) {
		if (!bitString.isEmpty() && !BIT_PATTERN.matcher(bitString).matches()) {
			throw new TypeErrorException("Input contains characters not of type " + BitType.INSTANCE + ": " + bitString + '.');
		}

		final List<IntegerStructImpl> bitList = new ArrayList<>(bitString.length());
		for (final char character : bitString.toCharArray()) {
			if (character == '0') {
				bitList.add(IntegerStructImpl.ZERO);
			} else if (character == '1') {
				bitList.add(IntegerStructImpl.ONE);
			}
		}
		return bitList;
	}

	public static BitVectorStruct valueOf(final String bitString) {
		return new BitVectorStructImpl(bitString);
	}

	public static BitVectorStruct valueOfCont(final List<IntegerStructImpl> contents) {
		return new BitVectorStructImpl(contents);
	}

	@Override
	public String toString() {
		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray) {
			stringBuilder.append("#*");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

			for (int i = 0; i < amountToPrint; i++) {
				final IntegerStructImpl integerStruct = contents.get(i);
				final String printedIntegerStruct = integerStruct.toString();

				stringBuilder.append(printedIntegerStruct);
			}
		} else {
			final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			stringBuilder.append(totalSize);

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
