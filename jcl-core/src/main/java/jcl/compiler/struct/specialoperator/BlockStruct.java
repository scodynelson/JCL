/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.icg.generator.ReturnFromException;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class BlockStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct name;

	private final PrognStruct forms;

	public BlockStruct(final SymbolStruct name, final List<LispStruct> forms) {
		super("block");
		this.name = name;
		this.forms = new PrognStruct(forms);
	}

	public SymbolStruct getName() {
		return name;
	}

	public PrognStruct getForms() {
		return forms;
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
	 * private LispStruct block_1(Environment var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      LispStruct var4;
	 *      try {
	 *          var4 = NILStruct.INSTANCE;
	 *      } catch (ReturnFromException var7) {
	 *          SymbolStruct var6 = var7.getName();
	 *          if(!var6.eq(var3)) {
	 *              throw var7;
	 *          }
	 *          var4 = var8.getResult();
	 *      }
	 *      return var4;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param environmentArgStore
	 * 		the storage location index on the stack where the {@link jcl.compiler.environment.Environment} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int environmentArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(name, generatorState, namePackageStore, nameSymbolStore);

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.RETURN_FROM_EXCEPTION_NAME);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		forms.generate(generatorState);

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
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.LISP_STRUCT_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_DESC,
		                   true);

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

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(BLOCK ");

		final String namePrinted = name.toString();
		builder.append(namePrinted);

		builder.append(' ');

		final String formsPrinted = forms.toString();
		builder.append(formsPrinted);

		builder.append(')');

		return builder.toString();
	}
}
