/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'throw' special operator code generation.
 */
@Component
final class ThrowCodeGenerator extends SpecialOperatorCodeGenerator<ThrowStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link ThrowStruct#catchTag} and {@link
	 * ThrowStruct#resultForm} values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Private constructor which passes 'throw' as the prefix value to be set in it's {@link #methodNamePrefix} value.
	 */
	private ThrowCodeGenerator() {
		super("throw");
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ThrowStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link ThrowStruct#catchTag} value</li>
	 * <li>Generating the {@link ThrowStruct#resultForm} value</li>
	 * <li>Creating and throwing a new {@link ThrowException} with the {@link LispStruct} catch tag and {@link
	 * LispStruct} result form values</li>
	 * </ol>
	 * As an example, it will transform {@code (throw 'foo 1)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct throw_1(Closure var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      BigInteger var5 = new BigInteger("1");
	 *      IntegerStruct var6 = new IntegerStruct(var5);
	 *      throw new ThrowException(var3, var6);
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link ThrowStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	protected void generateSpecialOperator(final ThrowStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		// Generate the Catch Tag
		final LispStruct catchTag = input.getCatchTag();
		codeGenerator.generate(catchTag, generatorState);

		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		// Generate the Result Form
		final LispStruct resultForm = input.getResultForm();
		codeGenerator.generate(resultForm, generatorState);

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
