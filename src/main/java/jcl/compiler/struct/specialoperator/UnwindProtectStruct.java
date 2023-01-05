/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import lombok.Getter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class UnwindProtectStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct protectedForm;
	private final PrognStruct cleanupForms;

	public UnwindProtectStruct(final LispStruct protectedForm, final List<LispStruct> cleanupForms) {
		super("unwindProtect");
		this.protectedForm = protectedForm;
		this.cleanupForms = new PrognStruct(cleanupForms);
	}

	/**
	 * {@inheritDoc} Generation method for {@code UnwindProtectStruct} objects, by performing the following operations:
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
	 * private LispStruct unwindProtect_1(Environment var1) {*
	 *      try {
	 *          BigInteger var2 = new BigInteger("1");
	 *          return new IntegerStruct(var2);
	 *      } finally {
	 *          BigInteger var3 = new BigInteger("2");
	 *          new IntegerStruct(var3);
	 *      }
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
