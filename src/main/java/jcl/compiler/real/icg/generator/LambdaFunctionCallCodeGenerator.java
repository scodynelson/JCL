/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform the generation of the code for anonymous lambda function calls, such as '((lambda ()))'.
 */
@Component
class LambdaFunctionCallCodeGenerator implements CodeGenerator<LambdaFunctionCallStruct> {

	/**
	 * {@link LambdaCodeGenerator} used for generating the {@link LambdaFunctionCallStruct#lambdaStruct} value.
	 */
	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link LambdaFunctionCallStruct#arguments} values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LambdaFunctionCallStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link LambdaFunctionCallStruct#lambdaStruct} value, creating the anonymous {@link
	 * LambdaStruct} class</li>
	 * <li>Generating a new array of {@link LispStruct} to be used as the function call arguments</li>
	 * <li>Generating each of the {@link LambdaFunctionCallStruct#arguments} values and adding them to the arguments
	 * array</li>
	 * <li>Generating the code to invoke the anonymous function call passing the created array of arguments</li>
	 * </ol>
	 * As an example, it will transform the anonymous lambda function call for {@code ((lambda ()))} into the following
	 * Java code:
	 * <pre>
	 * {@code
	 *      Lambda_10 var2 = new Lambda_10(var1);
	 *      LispStruct[] var3 = new LispStruct[0];
	 *      var2.apply(var3);
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link LambdaFunctionCallStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final LambdaFunctionCallStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LambdaStruct lambda = input.getLambdaStruct();
		lambdaCodeGenerator.generate(lambda, generatorState);

		final int functionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionStore);

		final List<LispStruct> arguments = input.getArguments();

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);

		final int argumentsArrayStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = methodBuilder.getNextAvailableStore();
		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			codeGenerator.generate(argument, generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, functionStore);
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.FUNCTION_STRUCT_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
				GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
				false);
	}
}
