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
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class MultipleValueProg1Struct extends CompilerSpecialOperatorStruct {

	private final LispStruct firstForm;
	private final PrognStruct forms;

	public MultipleValueProg1Struct(final LispStruct firstForm, final List<LispStruct> forms) {
		super("multipleValueProg1");
		this.firstForm = firstForm;
		this.forms = new PrognStruct(forms);
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
	 * private LispStruct multipleValueProg1_1(Environment var1) {
	 *      BigInteger var2 = new BigInteger("1");
	 *      IntegerStruct var3 = new IntegerStruct(var2);
	 *      BigInteger var4 = new BigInteger("2");
	 *      new IntegerStruct(var4);
	 *      return var3;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		firstForm.generate(generatorState);

		final int firstFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, firstFormStore);

		forms.generate(generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, firstFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
