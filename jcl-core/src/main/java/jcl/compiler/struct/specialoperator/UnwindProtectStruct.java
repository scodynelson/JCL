/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class UnwindProtectStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct protectedForm;

	private final PrognStruct cleanupForms;

	public UnwindProtectStruct(final LispStruct protectedForm, final List<LispStruct> cleanupForms) {
		super("unwindProtect");
		this.protectedForm = protectedForm;
		this.cleanupForms = new PrognStruct(cleanupForms);
	}

	public LispStruct getProtectedForm() {
		return protectedForm;
	}

	public PrognStruct getCleanupForms() {
		return cleanupForms;
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
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		// Start 'try{}'
		mv.visitLabel(tryBlockStart);

		protectedForm.generate(generatorState);

		final int protectedFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, protectedFormStore);

		// End 'try{}'
		mv.visitLabel(tryBlockEnd);

		// Non-exception 'cleanup forms' (aka. finally{})
		cleanupForms.generate(generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		// Start 'catch(Throwable t){}'
		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		cleanupForms.generate(generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		// End 'catch(Throwable t){}'
		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, protectedFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
