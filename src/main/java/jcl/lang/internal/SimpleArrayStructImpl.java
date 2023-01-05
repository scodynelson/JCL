package jcl.lang.internal;

import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link SimpleArrayStructImpl} is the object representation of a Lisp 'simple-array' type.
 */
public class SimpleArrayStructImpl extends AbstractMultiDimensionArrayStructImpl {

	/**
	 * The contents of the array.
	 */
	private final List<LispStruct> contents;

	/**
	 * Public constructor, initializing the dimensions, element-type, and contents.
	 *
	 * @param dimensions
	 * 		the array dimensions
	 * @param elementType
	 * 		the array elementType
	 * @param contents
	 * 		the array contents
	 */
	public SimpleArrayStructImpl(final List<IntegerStruct> dimensions, final LispStruct elementType,
	                             final List<LispStruct> contents) {
		super(dimensions, elementType);
		this.contents = contents;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return contents.get(rowMajorIndex);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		contents.set(rowMajorIndex, newElement);
		return newElement;
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final int validIndex = validateSubscript(index);
		return contents.get(validIndex);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) {
		final int validIndex = validateSubscript(index);
		contents.set(validIndex, newElement);
		return newElement;
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc} Generation method for {@code SimpleArrayStructImpl} objects, by performing the following
	 * operations:
	 * <ol>
	 * <li>Building the {@link #dimensions} values</li>
	 * <li>Building the {@link #elementType} values</li>
	 * <li>Building the {@link #contents} values, ensuring that each content value is treated as being
	 * 'quoted'</li>
	 * <li>Constructing a new SimpleArrayStructImpl with the built dimension and content {@link List}s</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int dimensionsStore = generateDimensions(generatorState);
		final int elementTypeStore = generateElementType(generatorState);
		final int contentsStore = generateContents(generatorState, contents);

		mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
		mv.visitVarInsn(Opcodes.ALOAD, elementTypeStore);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.ARRAY_STRUCT_NAME,
		                   GenerationConstants.ARRAY_STRUCT_TO_ARRAY_METHOD_NAME,
		                   GenerationConstants.ARRAY_STRUCT_TO_SIMPLE_ARRAY_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return ListStruct.toLispList(CommonLispSymbols.SIMPLE_ARRAY, elementType, arrayDimensions());
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.SIMPLE_ARRAY;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
