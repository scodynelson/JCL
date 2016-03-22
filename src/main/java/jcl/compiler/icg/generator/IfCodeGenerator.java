/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.LispStruct;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.ValuesStruct;
import jcl.compiler.struct.specialoperator.IfStruct;
import jcl.functions.Closure;
import jcl.symbols.NILStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'if' special operator code generation.
 */
@Component
final class IfCodeGenerator extends SpecialOperatorCodeGenerator<IfStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link IfStruct#testForm}, {@link IfStruct#thenForm},
	 * and {@link IfStruct#elseForm} values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@link NILCodeGenerator} used for generating a {@link NILStruct} when comparing the {@link IfStruct#testForm}.
	 */
	@Autowired
	private NILCodeGenerator nilCodeGenerator;

	/**
	 * Private constructor which passes 'if' as the prefix value to be set in it's {@link #methodNamePrefix} value.
	 */
	private IfCodeGenerator() {
		super("if");
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
	 *      if(!var2.equals(NILStruct.INSTANCE)) {
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
	 * @param input
	 * 		the {@link IfStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final IfStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct testForm = input.getTestForm();
		codeGenerator.generate(testForm, generatorState);

		final int testFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.VALUES_STRUCTS_NAME,
		                   GenerationConstants.VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_NAME,
		                   GenerationConstants.VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ASTORE, testFormStore);

		final Label elseStart = new Label();
		final Label elseEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, testFormStore);
		nilCodeGenerator.generate(NILStruct.INSTANCE, generatorState);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_OBJECT_NAME,
		                   GenerationConstants.JAVA_EQUALS_METHOD_NAME,
		                   GenerationConstants.JAVA_EQUALS_METHOD_DESC,
		                   false);
		mv.visitJumpInsn(Opcodes.IFNE, elseStart);

		final int resultFormStore = methodBuilder.getNextAvailableStore();

		final LispStruct thenForm = input.getThenForm();
		codeGenerator.generate(thenForm, generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitJumpInsn(Opcodes.GOTO, elseEnd);

		mv.visitLabel(elseStart);
		final LispStruct elseForm = input.getElseForm();
		codeGenerator.generate(elseForm, generatorState);
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitLabel(elseEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
