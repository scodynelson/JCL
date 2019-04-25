package jcl.lang.internal;

import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link ArrayStructImpl} is the object representation of a Lisp 'array' type.
 */
public abstract class ArrayStructImpl extends AbstractArrayStructImpl {

	protected boolean isAdjustable;

	protected ArrayStruct displacedTo;

	protected Integer displacedIndexOffset;

	protected ArrayStructImpl(final LispStruct elementType, final boolean isAdjustable) {
		super(elementType);

		this.isAdjustable = isAdjustable;
		displacedTo = null;
		displacedIndexOffset = 0;
	}

	protected ArrayStructImpl(final LispStruct elementType, final ArrayStruct displacedTo,
	                          final Integer displacedIndexOffset, final boolean isAdjustable) {
		super(elementType);

		this.isAdjustable = isAdjustable;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	@Override
	public boolean adjustableArrayP() {
		return isAdjustable;
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStruct.toLispInteger(displacedIndexOffset));
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ArrayStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link ArrayStruct#getDimensions()} values</li>
	 * <li>Building the {@link ArrayStruct#getContents()} values, ensuring that each content value is treated as being
	 * 'quoted'</li>
	 * <li>Constructing a new {@link ArrayStruct} with the built dimension and content {@link List}s</li>
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
		final int dimensionsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, dimensionsStore);

		final List<Integer> dimensions = getDimensions();
		for (final Integer dimension : dimensions) {
			mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
			mv.visitLdcInsn(dimension);
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

		final List<LispStruct> contents = getContents();
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

		mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.ARRAY_STRUCT_NAME,
		                   GenerationConstants.ARRAY_STRUCT_TO_ARRAY_METHOD_NAME,
		                   GenerationConstants.ARRAY_STRUCT_TO_ARRAY_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		// TODO: Simple vs Not???
		return ListStruct.toLispList(CommonLispSymbols.ARRAY, elementType, arrayDimensions());
	}

	@Override
	public ClassStruct classOf() {
		// TODO: Simple vs Not???
		return BuiltInClassStruct.ARRAY;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		// TODO: Simple vs Not???
		if (typeSpecifier == CommonLispSymbols.ARRAY) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.ARRAY) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
