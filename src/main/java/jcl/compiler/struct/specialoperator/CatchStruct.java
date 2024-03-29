/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.icg.generator.ThrowException;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import lombok.Getter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class CatchStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct catchTag;
	private final PrognStruct forms;

	public CatchStruct(final LispStruct catchTag, final List<LispStruct> forms) {
		super("catch");
		this.catchTag = catchTag;
		this.forms = new PrognStruct(forms);
	}

	/**
	 * {@inheritDoc} Generation method for {@code CatchStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link CatchStruct#catchTag} value</li>
	 * <li>Initializing a try-catch block</li>
	 * <li>Generating each of the {@link CatchStruct#forms} inside the try block, ensuring to store the final result
	 * into a variable</li>
	 * <li>Catching the expected {@link ThrowException}</li>
	 * <li>Grabbing the {@link ThrowException#catchTag} {@link LispStruct} and comparing it for equality against the
	 * previously fetched {@link CatchStruct#catchTag}</li>
	 * <li>If the {@link LispStruct}s are equal, the final result variable is assigned the {@link
	 * ThrowException#resultForm} value</li>
	 * <li>If the {@link LispStruct}s are not equal, the {@link ThrowException} is re-thrown</li>
	 * </ol>
	 * As an example, it will transform {@code (catch 'foo)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct catch_1(Environment var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      LispStruct var5;
	 *      try {
	 *          var5 = NILStruct.INSTANCE;
	 *      } catch (ThrowException var8) {
	 *          LispStruct var7 = var8.getCatchTag();
	 *          if(!var7.eq(var3)) {
	 *              throw var8;
	 *          }
	 *          var5 = var8.getResultForm();
	 *      }
	 *      return var5;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 *        {@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		catchTag.generate(generatorState);
		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.THROW_EXCEPTION_NAME);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		forms.generate(generatorState);

		final int resultFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		// End 'try{}'
		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		// Start 'catch(ThrowException te){}'
		mv.visitLabel(catchBlockStart);
		final int throwExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.THROW_EXCEPTION_NAME,
		                   GenerationConstants.THROW_EXCEPTION_GET_CATCH_TAG_METHOD_NAME,
		                   GenerationConstants.THROW_EXCEPTION_GET_CATCH_TAG_METHOD_DESC,
		                   false);
		final int throwExceptionCatchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, throwExceptionCatchTagStore);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionCatchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.LISP_STRUCT_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_DESC,
		                   true);

		final Label rethrowException = new Label();
		mv.visitJumpInsn(Opcodes.IFEQ, rethrowException);

		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.THROW_EXCEPTION_NAME,
		                   GenerationConstants.THROW_EXCEPTION_GET_RESULT_FORM_METHOD_NAME,
		                   GenerationConstants.THROW_EXCEPTION_GET_RESULT_FORM_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(rethrowException);
		mv.visitVarInsn(Opcodes.ALOAD, throwExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		// End 'catch(ThrowException te){}'
		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
