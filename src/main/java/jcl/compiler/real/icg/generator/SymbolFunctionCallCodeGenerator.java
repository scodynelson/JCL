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
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.SymbolFunctionCallStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform the generation of the code for anonymous lambda function calls, such as '(+ 1)'.
 */
@Component
class SymbolFunctionCallCodeGenerator implements CodeGenerator<SymbolFunctionCallStruct> {

	/**
	 * {@link SymbolFunctionCodeGenerator} for generating the {@link SymbolFunctionCallStruct#symbolCompilerFunction}
	 * when the value of {@link SymbolFunctionCallStruct#isRecursiveCall} is false.
	 */
	@Autowired
	private SymbolFunctionCodeGenerator symbolFunctionCodeGenerator;

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link SymbolFunctionCallStruct#arguments} values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link SymbolFunctionCallStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link SymbolFunctionCallStruct#symbolCompilerFunction} value, if the {@link
	 * SymbolFunctionCallStruct#isRecursiveCall} is false</li>
	 * <li>Generating a new array of {@link LispStruct} to be used as the function call arguments</li>
	 * <li>Generating each of the {@link SymbolFunctionCallStruct#arguments} values and adding them to the arguments
	 * array</li>
	 * <li>Generating the code to invoke the function call passing the created array of arguments</li>
	 * </ol>
	 * As an example, it will transform the function call for {@code (+ 1)} into the following
	 * Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP");
	 *      SymbolStruct var3 = var2.findSymbol("+").getSymbol();
	 *      FunctionStruct var4 = var3.getFunction();
	 *      LispStruct[] var5 = new LispStruct[1];
	 *      BigInteger var7 = new BigInteger("1");
	 *      IntegerStruct var6 = new IntegerStruct(var7);
	 *      var5[0] = var6;
	 *      var4.apply(var5);
	 * }
	 * </pre>
	 * NOTE: If the value of {@link SymbolFunctionCallStruct#isRecursiveCall} is true, the current lambda will be used
	 * via 'this' reference to invoke a recursive call, rather than re-retrieving the {@link
	 * SymbolStruct#getFunction()} value
	 *
	 * @param input
	 * 		the {@link SymbolFunctionCallStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final SymbolFunctionCallStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final boolean recursiveCall = input.isRecursiveCall();

		final int functionStore;
		if (recursiveCall) {
			functionStore = 0;
		} else {
			final SymbolCompilerFunctionStruct functionSymbol = input.getSymbolCompilerFunction();
			symbolFunctionCodeGenerator.generate(functionSymbol, generatorState);

			functionStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, functionStore);
		}

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
