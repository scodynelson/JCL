/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ValuesStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class IfStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct testForm;

	private final LispStruct thenForm;

	private final LispStruct elseForm;

	public IfStruct(final LispStruct testForm, final LispStruct thenForm, final LispStruct elseForm) {
		super("if");
		this.testForm = testForm;
		this.thenForm = thenForm;
		this.elseForm = elseForm;
	}

	public LispStruct getTestForm() {
		return testForm;
	}

	public LispStruct getThenForm() {
		return thenForm;
	}

	public LispStruct getElseForm() {
		return elseForm;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link IfStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link IfStruct#testForm}</li>
	 * <li>Retrieving the primary value via {@link ValuesStruct#getPrimaryValue()} if the generated test form is a
	 * {@link ValuesStruct}</li>
	 * <li>Generating the equality test for the test form against {@link NILStruct#INSTANCE}</li>
	 * <li>Generating the {@link IfStruct#thenForm}</li>
	 * <li>Generating the {@link IfStruct#elseForm}</li>
	 * </ol>
	 * As an example, it will transform {@code (if t 1 2)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct if_1(Closure var1) {
	 *      LispStruct var2 = TStruct.INSTANCE;
	 *      if(var2 instanceof ValuesStruct) {
	 *          ValuesStruct var3 = (ValuesStruct)var2;
	 *          var2 = var3.getPrimaryValue();
	 *      }
	 *      IntegerStruct var4;
	 *      if(!var2.eq(NILStruct.INSTANCE)) {
	 *          BigInteger var5 = new BigInteger("1");
	 *          var4 = new IntIntegerStruct(var5);
	 *      } else {
	 *          BigInteger var6 = new BigInteger("2");
	 *          var4 = new IntIntegerStruct(var6);
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
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		testForm.generate(generatorState);

		final int testFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.VALUES_STRUCT_NAME,
		                   GenerationConstants.VALUES_STRUCT_EXTRACT_PRIMARY_VALUE_METHOD_NAME,
		                   GenerationConstants.VALUES_STRUCT_EXTRACT_PRIMARY_VALUE_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		final Label elseStart = new Label();
		final Label elseEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		NILStruct.INSTANCE.generate(generatorState);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.LISP_STRUCT_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_EQ_METHOD_DESC,
		                   true);
		mv.visitJumpInsn(Opcodes.IFNE, elseStart);

		final int resultFormStore = methodBuilder.getNextAvailableStore();

		thenForm.generate(generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitJumpInsn(Opcodes.GOTO, elseEnd);

		mv.visitLabel(elseStart);
		elseForm.generate(generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(elseEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(IF ");

		final String testFormPrinted = testForm.toString();
		builder.append(testFormPrinted);
		builder.append(' ');
		final String thenFormPrinted = thenForm.toString();
		builder.append(thenFormPrinted);
		builder.append(' ');
		final String elseFormPrinted = elseForm.toString();
		builder.append(elseFormPrinted);
		builder.append(')');

		return builder.toString();
	}
}
