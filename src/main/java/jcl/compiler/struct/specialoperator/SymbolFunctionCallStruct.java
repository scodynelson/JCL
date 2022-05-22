/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Class to perform the generation of the code for anonymous lambda function calls, such as '(+ 1)'.
 */
@Getter
public class SymbolFunctionCallStruct extends CompilerSpecialOperatorStruct {

	private final SymbolCompilerFunctionStruct symbolCompilerFunction;
	private final List<LispStruct> arguments;
	private final boolean isRecursiveCall;

	public SymbolFunctionCallStruct(final SymbolCompilerFunctionStruct symbolCompilerFunction, final List<LispStruct> arguments,
	                                final boolean isRecursiveCall) {
		super("symbolFunctionCall");
		this.symbolCompilerFunction = symbolCompilerFunction;
		this.arguments = arguments;
		this.isRecursiveCall = isRecursiveCall;
	}

	@Override
	public String toString() {

		final StringBuilder builder = new StringBuilder("(");

		final SymbolStruct functionSymbol = symbolCompilerFunction.getFunctionSymbol();
		final String functionSymbolPrinted = functionSymbol.toString();
		builder.append(functionSymbolPrinted);
		builder.append(' ');

		final String printedArguments =
				arguments.stream()
				         .map(Object::toString)
				         .collect(Collectors.joining(" "));
		builder.append(printedArguments);
		builder.append(')');

		return builder.toString();
	}

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
	 * private LispStruct symbolFunctionCall_1(Environment var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP");
	 *      SymbolStruct var3 = var2.findSymbol("+").getSymbol();
	 *      FunctionStructImpl var4 = var1.getFunction(var3);
	 *      LispStruct[] var5 = new LispStruct[1];
	 *      BigInteger var7 = new BigInteger("1");
	 *      IntegerStruct var6 = new IntegerStruct(var7);
	 *      var5[0] = var6;
	 *      return var4.apply(var5);
	 * }
	 * }
	 * </pre>
	 * NOTE: If the value of {@link SymbolFunctionCallStruct#isRecursiveCall} is true, the current lambda will be used
	 * via 'this' reference to invoke a recursive call, rather than re-retrieving the {@link
	 * jcl.compiler.environment.Environment#getFunction(SymbolStruct)} value
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int functionStore;
		if (isRecursiveCall) {
			functionStore = 0;
		} else {
			symbolCompilerFunction.generate(generatorState);

			functionStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, functionStore);
		}

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);
		final int argumentsArrayStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = methodBuilder.getNextAvailableStore();

		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			argument.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, functionStore);
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.FUNCTION_STRUCT_NAME,
		                   GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
		                   GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
		                   true);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
