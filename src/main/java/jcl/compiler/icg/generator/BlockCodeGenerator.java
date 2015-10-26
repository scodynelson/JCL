/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.BlockStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.functions.Closure;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'block' special operator code generation.
 */
@Component
final class BlockCodeGenerator extends SpecialOperatorCodeGenerator<BlockStruct> {

	/**
	 * {@link PrognCodeGenerator} used for generating the {@link BlockStruct#forms}.
	 */
	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	/**
	 * Private constructor which passes 'block' as the prefix value to be set in it's {@link #methodNamePrefix} value.
	 */
	private BlockCodeGenerator() {
		super("block");
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link BlockStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Fetching the global 'COMMON-LISP-USER' package</li>
	 * <li>Finding the {@link SymbolStruct} with the {@link BlockStruct#name} value in the fetched 'COMMON-LISP-USER'
	 * package</li>
	 * <li>Initializing a try-catch block</li>
	 * <li>Generating each of the {@link BlockStruct#forms} inside the try block, ensuring to store the final result
	 * into a variable</li>
	 * <li>Catching the expected {@link ReturnFromException}</li>
	 * <li>Grabbing the {@link ReturnFromException#name} {@link SymbolStruct} and comparing it for equality against the
	 * previously fetched {@link BlockStruct#name}</li>
	 * <li>If the {@link SymbolStruct}s are equal, the final result variable is assigned the {@link
	 * ReturnFromException#result} value</li>
	 * <li>If the {@link SymbolStruct}s are not equal, the {@link ReturnFromException} is re-thrown</li>
	 * </ol>
	 * As an example, it will transform {@code (block foo)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct block_1(Closure var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      LispStruct var4;
	 *      try {
	 *          var4 = NullStruct.INSTANCE;
	 *      } catch (ReturnFromException var7) {
	 *          SymbolStruct var6 = var7.getName();
	 *          if(!var6.equals(var3)) {
	 *              throw var7;
	 *          }
	 *          var4 = var8.getResult();
	 *      }
	 *      return var4;
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link BlockStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final BlockStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		final SymbolStruct<?> name = input.getName();
		CodeGenerators.generateSymbol(name, methodBuilder, namePackageStore, nameSymbolStore);

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.RETURN_FROM_EXCEPTION_NAME);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, generatorState);

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		// End 'try{}'
		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		// Start 'catch(ReturnFromException rfe){}'
		mv.visitLabel(catchBlockStart);
		final int returnFromExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, returnFromExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_NAME_METHOD_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_NAME_METHOD_DESC,
				false);
		final int returnFromExceptionNameStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, returnFromExceptionNameStore);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionNameStore);
		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);

		final Label rethrowException = new Label();
		mv.visitJumpInsn(Opcodes.IFEQ, rethrowException);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(rethrowException);
		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		// End 'catch(ReturnFromException rfe){}'
		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
