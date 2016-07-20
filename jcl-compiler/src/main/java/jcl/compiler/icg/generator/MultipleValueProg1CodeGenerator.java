/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.MultipleValueProg1Struct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.lang.LispStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
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
	 * Private constructor which passes 'multipleValueProg1' as the prefix value to be set in it's {@link
	 * #methodNamePrefix} value.
	 */
	private MultipleValueProg1CodeGenerator() {
		super("multipleValueProg1");
	}

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<MultipleValueProg1Struct> event) {
		super.onGeneratorEvent(event);
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
	 *      IntegerStruct var3 = new IntIntegerStruct(var2);
	 *      BigInteger var4 = new BigInteger("2");
	 *      new IntIntegerStruct(var4);
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
		codeGenerator.generate(forms, generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, firstFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
