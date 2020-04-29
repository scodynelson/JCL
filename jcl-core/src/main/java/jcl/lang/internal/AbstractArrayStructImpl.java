package jcl.lang.internal;

import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Abstract base class for 'array' types.
 */
public abstract class AbstractArrayStructImpl extends LispStructImpl implements ArrayStruct {

	/**
	 * The total size of the 'array'.
	 */
	protected LispStruct elementType;

	/**
	 * Protected constructor, initializing the element-type.
	 *
	 * @param elementType
	 * 		the element-type of the 'array'
	 */
	protected AbstractArrayStructImpl(final LispStruct elementType) {
		this.elementType = elementType;
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public LispStruct arrayElementType() {
		return elementType;
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return ListStruct.toLispList(CommonLispSymbols.ARRAY, elementType, arrayDimensions());
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.ARRAY;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.ARRAY) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.ARRAY) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	protected int generateElementType(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int elementTypeStore = methodBuilder.getNextAvailableStore();
		final QuoteStruct quotedContent = new QuoteStruct(elementType);
		quotedContent.generate(generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, elementTypeStore);

		return elementTypeStore;
	}

	protected static int generateContents(final GeneratorState generatorState,
	                                      final List<? extends LispStruct> contents) {
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

		return contentsStore;
	}
}
