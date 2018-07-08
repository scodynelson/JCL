package jcl.lang.internal;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.lang.ArrayStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.PrinterVariables;
import jcl.type.LispType;
import jcl.type.SimpleVectorType;
import jcl.type.TType;
import jcl.type.VectorType;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link VectorStructImpl} is the object representation of a Lisp 'vector' type.
 */
public class VectorStructImpl extends AbstractVectorStructImpl {

	private List<LispStruct> contents;

	protected Integer fillPointer;

	protected boolean isAdjustable;

	protected ArrayStruct displacedTo;

	protected Integer displacedIndexOffset;

	protected VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                           final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, elementType, size);
		this.fillPointer = fillPointer;
		this.isAdjustable = isAdjustable;
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
	public VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                        final List<LispStruct> contents, final boolean isAdjustable, final Integer fillPointer) {
		this(vectorType, size, elementType, isAdjustable, fillPointer);
		this.contents = contents;
	}

	/**
	 * Protected constructor.
	 *
	 * @param vectorType
	 * 		the vector type
	 * @param size
	 * 		the vector size
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param elementType
	 * 		the vector elementType
	 * @param isAdjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 */
	public VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                        final ArrayStruct displacedTo, final Integer displacedIndexOffset,
	                        final boolean isAdjustable, final Integer fillPointer) {
		this(vectorType, size, elementType, isAdjustable, fillPointer);
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	/*
		Old Builders
	 */

	public static VectorStruct valueOf(final List<LispStruct> contents) {
		return new VectorStructImpl(SimpleVectorType.INSTANCE,
		                            contents.size(),
		                            TType.INSTANCE,
		                            contents,
		                            false,
		                            null);
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public IntegerStruct fillPointer() {
		if (fillPointer == null) {
			throw new TypeErrorException("VECTOR has no fill-pointer to retrieve.");
		}
		return IntegerStruct.toLispInteger(fillPointer);
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		final int intValue = fillPointer.toJavaInt();
		if ((intValue < 0) || (intValue > totalSize)) {
			throw new ErrorException(
					"Fill-pointer " + fillPointer + " value is out of bounds for VECTOR with size " + totalSize + '.');
		}

		this.fillPointer = intValue;
		return fillPointer;
	}

	@Override
	public LispStruct vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a VECTOR with no fill-pointer.");
		}
		if (fillPointer == 0) {
			throw new ErrorException("Nothing left to pop.");
		}

		final LispStruct element = contents.get(--fillPointer);
		contents.set(fillPointer, null);
		return element;
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) { // TODO: type check
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (fillPointer >= contents.size()) {
			return NILStruct.INSTANCE;
		}

		final Integer formerFillPointer = fillPointer++;
		contents.set(formerFillPointer, newElement);
		return IntegerStruct.toLispInteger(formerFillPointer);
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement,
	                                      final IntegerStruct extension) { // TODO: type check
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (!isAdjustable) {
			throw new TypeErrorException("VECTOR is not adjustable.");
		}
		if (fillPointer >= contents.size()) {
//			adjustArray(fillPointer + extensionAmount); // TODO
		}

		final Integer formerFillPointer = fillPointer++;
		contents.set(formerFillPointer, newElement);
		return IntegerStruct.toLispInteger(formerFillPointer);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final LispStruct initialElement, final IntegerStruct fillPointer) {

		if (!dimensions.isEmpty()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		if (this.elementType.isNotOfType(upgradedET)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		final LispType initialElementType = initialElement.getType();
		if (initialElementType.isNotOfType(upgradedET)) {
			throw new TypeErrorException(
					"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = upgradedET;
			contents = Stream.generate(() -> initialElement)
			                 .limit(size.toJavaInt())
			                 .collect(Collectors.toList());
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return VectorStruct.builder(size)
			                   .elementType(upgradedET)
			                   .initialElement(initialElement)
			                   .build();
		}
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final SequenceStruct initialContents, final IntegerStruct fillPointer) {

		if (!dimensions.isEmpty()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		if (this.elementType.isNotOfType(upgradedET)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		for (final LispStruct initialElement : initialContents) {
			final LispType initialElementType = initialElement.getType();
			if (initialElementType.isNotOfType(upgradedET)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = upgradedET;
			final List<Integer> dimensionInts = Collections.singletonList(size.toJavaInt());
			contents = ArrayStruct.getValidContents(dimensionInts, elementType, initialContents);
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return VectorStruct.builder(size)
			                   .elementType(upgradedET)
			                   .initialContents(initialContents)
			                   .build();
		}
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final IntegerStruct fillPointer, final ArrayStruct displacedTo,
	                               final IntegerStruct displacedIndexOffset) {

		if (!dimensions.isEmpty()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		if (this.elementType.isNotOfType(upgradedET)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		final LispType initialElementType = displacedTo.arrayElementType();
		if (initialElementType.isNotOfType(upgradedET)) {
			throw new TypeErrorException(
					"Provided array for displacement " + displacedTo + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		try {
			displacedTo.rowMajorAref(displacedIndexOffset);
		} catch (final ErrorException ignored) {
			throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = elementType;
			contents = null;
			this.displacedTo = displacedTo;
			this.displacedIndexOffset = displacedIndexOffset.toJavaInt();
			return this;
		} else {
			return VectorStruct.builder(size)
			                   .elementType(upgradedET)
			                   .displacedTo(displacedTo)
			                   .displacedIndexOffset(displacedIndexOffset)
			                   .build();
		}
	}

	@Override
	public boolean adjustableArrayP() {
		return isAdjustable;
	}

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		final int rowMajorIndex = validateSubscript(subscript);
		if (displacedTo == null) {
			return contents.get(rowMajorIndex);
		}

		final IntegerStruct indexToGet = IntegerStruct.toLispInteger(displacedIndexOffset + rowMajorIndex);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) { // TODO: type check
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		final int rowMajorIndex = validateSubscript(subscript);
		if (displacedTo == null) {
			contents.set(rowMajorIndex, newElement);
		} else {
			final IntegerStruct indexToSet = IntegerStruct.toLispInteger(displacedIndexOffset + rowMajorIndex);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	@Override
	public boolean arrayHasFillPointerP() {
		return fillPointer != null;
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStruct.toLispInteger(displacedIndexOffset));
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return contents.get(indexInt);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) { // TODO: type check
		final int indexInt = validateSubscript(index);
		contents.set(indexInt, newElement);
		return newElement;
	}

// =================

	@Override
	public List<LispStruct> getContents() {
		return contents;
	}

// =================

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public IntegerStruct length() {
		if (fillPointer != null) {
			return IntegerStruct.toLispInteger(fillPointer);
		}
		return IntegerStruct.toLispInteger(totalSize);
	}

	@Override
	public LispStruct elt(final IntegerStruct index) {
		final int indexInt = validateIndexAgainstFillPointer(index);
		return contents.get(indexInt);
	}

	@Override
	public LispStruct setfElt(final LispStruct newElement, final IntegerStruct index) { // TODO: type check
		final int indexInt = validateIndexAgainstFillPointer(index);
		contents.set(indexInt, newElement);
		return newElement;
	}

	protected int validateIndexAgainstFillPointer(final IntegerStruct index) {
		if (fillPointer != null) {
			final int indexInt = index.toJavaInt();
			if (indexInt > fillPointer) {
				throw new ErrorException(index + " is not a valid sequence index for " + this);
			}
		}
		return validateSubscript(index);
	}

	@Override
	public VectorStruct reverse() {
		return this; // TODO
	}

	@Override
	public VectorStruct nReverse() {
		return this; // TODO
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

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link VectorStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link VectorStruct#getContents()}, ensuring that each content value is treated as being
	 * 'quoted'</li>
	 * <li>Constructing a new {@link VectorStruct} with the built content {@link List}</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_ARRAY_LIST_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
		                   false);
		final int contentsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, contentsStore);

		final int contentStore = methodBuilder.getNextAvailableStore();

		for (final LispStruct content : contents) {
			final QuoteStruct quotedContent = new QuoteStruct(content);
			quotedContent.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, contentStore);

			mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
			mv.visitVarInsn(Opcodes.ALOAD, contentStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_LIST_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
			                   GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_VECTOR_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_VECTOR_METHOD_DESC,
		                   false);
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().toJavaPBoolean();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().toJavaPBoolean();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append("#(");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

			for (int i = 0; i < amountToPrint; i++) {
				final LispStruct lispStruct = contents.get(i);
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
