/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.icg.generator.ThrowException;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class ThrowStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct catchTag;
	private final LispStruct resultForm;

	public ThrowStruct(final LispStruct catchTag, final LispStruct resultForm) {
		super("throw");
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}

	/**
	 * {@inheritDoc} Generation method for {@code ThrowStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link ThrowStruct#catchTag} value</li>
	 * <li>Generating the {@link ThrowStruct#resultForm} value</li>
	 * <li>Creating and throwing a new {@link ThrowException} with the {@link LispStruct} catch tag and {@link
	 * LispStruct} result form values</li>
	 * </ol>
	 * As an example, it will transform {@code (throw 'foo 1)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct throw_1(Environment var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      BigInteger var5 = new BigInteger("1");
	 *      IntegerStruct var6 = new IntegerStruct(var5);
	 *      throw new ThrowException(var3, var6);
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

		// Generate the Catch Tag
		catchTag.generate(generatorState);

		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		// Generate the Result Form
		resultForm.generate(generatorState);

		final int resultFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		// Create and throw the ThrowException
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.THROW_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.THROW_EXCEPTION_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.THROW_EXCEPTION_INIT_DESC,
		                   false);
		mv.visitInsn(Opcodes.ATHROW);
	}
}
