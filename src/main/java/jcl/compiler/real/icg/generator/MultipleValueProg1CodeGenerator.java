/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.MultipleValueProg1Struct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.functions.Closure;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'multiple-value-prog1' special operator code generation.
 */
@Component
final class MultipleValueProg1CodeGenerator extends SpecialOperatorCodeGenerator<MultipleValueProg1Struct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link MultipleValueProg1Struct#firstForm} value.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@link PrognCodeGenerator} used for generating the values of the {@link MultipleValueProg1Struct#forms}.
	 */
	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	/**
	 * Private constructor which passes 'multipleValueProg1' as the prefix value to be set in it's {@link
	 * #methodNamePrefix} value.
	 */
	private MultipleValueProg1CodeGenerator() {
		super("multipleValueProg1");
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link MultipleValueProg1Struct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link MultipleValueProg1Struct#firstForm} value</li>
	 * <li>Generating the {@link MultipleValueProg1Struct#forms} values</li>
	 * <li>Returning the {@link MultipleValueProg1Struct#firstForm} generated value</li>
	 * </ol>
	 * As an example, it will transform {@code (multiple-value-prog1 1 2)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct multipleValueProg1_1(Closure var1) {
	 *      BigInteger var2 = new BigInteger("1");
	 *      IntegerStruct var3 = new IntegerStruct(var2);
	 *      BigInteger var4 = new BigInteger("2");
	 *      new IntegerStruct(var4);
	 *      return var3;
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link MultipleValueProg1Struct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final MultipleValueProg1Struct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct firstForm = input.getFirstForm();
		codeGenerator.generate(firstForm, generatorState);

		final int firstFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, firstFormStore);

		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, firstFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
