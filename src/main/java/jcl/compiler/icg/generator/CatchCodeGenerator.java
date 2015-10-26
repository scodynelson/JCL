/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.LispStruct;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.CatchStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.functions.Closure;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'catch' special operator code generation.
 */
@Component
final class CatchCodeGenerator extends SpecialOperatorCodeGenerator<CatchStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link CatchStruct#catchTag}.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@link PrognCodeGenerator} used for generating the {@link CatchStruct#forms}.
	 */
	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	/**
	 * Private constructor which passes 'catch' as the prefix value to be set in it's {@link #methodNamePrefix} value.
	 */
	private CatchCodeGenerator() {
		super("catch");
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link CatchStruct} objects, by performing the following operations:
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
	 * private LispStruct catch_1(Closure var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      LispStruct var5;
	 *      try {
	 *          var5 = NullStruct.INSTANCE;
	 *      } catch (ThrowException var8) {
	 *          LispStruct var7 = var8.getCatchTag();
	 *          if(!var7.equals(var3)) {
	 *              throw var8;
	 *          }
	 *          var5 = var8.getResultForm();
	 *      }
	 *      return var5;
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link CatchStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final CatchStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct catchTag = input.getCatchTag();
		codeGenerator.generate(catchTag, generatorState);
		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.THROW_EXCEPTION_NAME);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, generatorState);

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
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_OBJECT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);

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
