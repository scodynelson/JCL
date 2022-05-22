package jcl.lang.internal;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.condition.exception.ErrorException;
import org.apache.commons.math3.exception.DimensionMismatchException;
import org.apache.commons.math3.exception.OutOfRangeException;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Abstract base class for 'array' types that contain multiple dimensions.
 */
public abstract class AbstractMultiDimensionArrayStructImpl extends AbstractArrayStructImpl {

	/**
	 * The {@link MultidimensionalCounter} used to traverse the multi-dimensional array.
	 */
	protected final MultidimensionalCounter multidimensionalCounter;

	/**
	 * The dimensions of the array.
	 */
	protected final List<IntegerStruct> dimensions;

	/**
	 * Public constructor, initializing the dimensions, element-type, and contents.
	 *
	 * @param dimensions
	 * 		the array size
	 * @param elementType
	 * 		the array elementType
	 */
	protected AbstractMultiDimensionArrayStructImpl(final List<IntegerStruct> dimensions,
	                                                final LispStruct elementType) {
		super(elementType);
		this.dimensions = dimensions;

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(IntegerStruct::toJavaInt)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		if (dimensions.isEmpty()) {
			throw new ErrorException("Cannot determine array dimension for array with rank 0.");
		}

		final int axisInt = axisNumber.toJavaInt();
		final int[] sizes = multidimensionalCounter.getSizes();
		if ((axisInt < 0) || (axisInt >= sizes.length)) {
			throw new ErrorException("Subscript " + axisNumber + " is out of bounds for " + this + '.');
		}
		return IntegerStruct.toLispInteger(sizes[axisInt]);
	}

	@Override
	public ListStruct arrayDimensions() {
		final int[] sizes = multidimensionalCounter.getSizes();
		final List<IntegerStruct> dimensionStructs
				= Arrays.stream(sizes)
				        .mapToObj(IntegerStruct::toLispInteger)
				        .collect(Collectors.toList());
		return ListStruct.toLispList(dimensionStructs);
	}

	@Override
	public IntegerStruct arrayRank() {
		final int rank = multidimensionalCounter.getDimension();
		return IntegerStruct.toLispInteger(rank);
	}

	@Override
	public IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return IntegerStruct.toLispInteger(rowMajorIndex);
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		final int totalSize = multidimensionalCounter.getSize();
		return IntegerStruct.toLispInteger(totalSize);
	}

	protected int rowMajorIndexInternal(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;

		final int rank = multidimensionalCounter.getDimension();
		if (numberOfSubscripts != rank) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank " + rank + '.');
		}

		final int[] intSubscripts
				= Arrays.stream(subscripts)
				        .mapToInt(IntegerStruct::toJavaInt)
				        .toArray();

		final int rowMajorIndex;
		try {
			rowMajorIndex = multidimensionalCounter.getCount(intSubscripts);
		} catch (final DimensionMismatchException | OutOfRangeException ex) {
			final Number argument = ex.getArgument();
			throw new ErrorException("Subscript " + argument + " is out of bounds for " + this + '.');
		}
		return rowMajorIndex;
	}

	/**
	 * Validates the provided subscript is a valid subscript for retrieving a value from the 'array', meaning the value
	 * is not less-than 0 and is not greater-than-or-equal-to the total size of the 'array'.
	 *
	 * @param subscript
	 * 		the subscript value to validate
	 *
	 * @return the subscript value
	 *
	 * @throws ErrorException
	 * 		if the subscript value is not valid
	 */
	protected int validateSubscript(final IntegerStruct subscript) {
		final int subscriptInt = subscript.toJavaInt();
		final int totalSize = multidimensionalCounter.getSize();
		if ((subscriptInt < 0) || (subscriptInt >= totalSize)) {
			throw new ErrorException("Subscript " + subscript + " is out of bounds for " + this + '.');
		}
		return subscriptInt;
	}

	/*
	LISP-STRUCT
	 */

	protected int generateDimensions(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_ARRAY_LIST_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
		                   false);
		final int dimensionsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, dimensionsStore);

		for (final IntegerStruct dimension : dimensions) {
			mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
			mv.visitLdcInsn(dimension.toJavaInteger());
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.JAVA_INTEGER_NAME,
			                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_NAME,
			                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_DESC,
			                   false);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_LIST_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);
		}

		return dimensionsStore;
	}
}
