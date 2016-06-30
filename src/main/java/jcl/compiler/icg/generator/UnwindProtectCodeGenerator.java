/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.LispStruct;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.UnwindProtectStruct;
import jcl.functions.Closure;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'unwind-protect' special operator code generation.
 */
@Component
final class UnwindProtectCodeGenerator extends SpecialOperatorCodeGenerator<UnwindProtectStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link UnwindProtectStruct#protectedForm}.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Private constructor which passes 'unwindProtect' as the prefix value to be set in it's {@link #methodNamePrefix}
	 * value.
	 */
	private UnwindProtectCodeGenerator() {
		super("unwindProtect");
	}

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<UnwindProtectStruct> event) {
		super.onGeneratorEvent(event);
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link UnwindProtectStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Initializing a try-catch block</li>
	 * <li>Generating the {@link UnwindProtectStruct#protectedForm} inside the try block, ensuring to store the final
	 * result into a variable</li>
	 * <li>Generate the {@link UnwindProtectStruct#cleanupForms} as part of the error free 'finally'</li>
	 * <li>Generate the {@link UnwindProtectStruct#cleanupForms} as part of the error caught 'finally', ensuring that
	 * the error caught is re-thrown</li>
	 * </ol>
	 * As an example, it will transform {@code (unwind-protect 1 2)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct unwindProtect_1(Closure var1) {*
	 *      try {
	 *          BigInteger var2 = new BigInteger("1");
	 *          return new IntIntegerStruct(var2);
	 *      } finally {
	 *          BigInteger var3 = new BigInteger("2");
	 *          new IntIntegerStruct(var3);
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link UnwindProtectStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final UnwindProtectStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		final LispStruct protectedForm = input.getProtectedForm();
		codeGenerator.generate(protectedForm, generatorState);

		final int protectedFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, protectedFormStore);

		// End 'try{}'
		mv.visitLabel(tryBlockEnd);

		// Non-exception 'cleanup forms' (aka. finally{})
		final PrognStruct cleanupForms = input.getCleanupForms();
		codeGenerator.generate(cleanupForms, generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		// Start 'catch(Throwable t){}'
		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		codeGenerator.generate(cleanupForms, generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		// End 'catch(Throwable t){}'
		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, protectedFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
