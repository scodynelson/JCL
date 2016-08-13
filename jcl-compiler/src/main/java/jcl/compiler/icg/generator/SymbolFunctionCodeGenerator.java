/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.FunctionStructImpl;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform the generation of the code for anonymous lambda function objects, such as '#'(lambda ())'.
 */
@Component
final class SymbolFunctionCodeGenerator implements CodeGenerator<SymbolCompilerFunctionStruct> {

	/**
	 * {@inheritDoc}
	 * Generation method for {@link SymbolCompilerFunctionStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link SymbolCompilerFunctionStruct#functionSymbol} value</li>
	 * <li>Generating the code to retrieve the {@link SymbolStruct#getFunction()} call to retrieve the associated
	 * {@link FunctionStructImpl} associated with the function symbol</li>
	 * </ol>
	 * As an example, it will transform the function symbol '+' for {@code (+ 1)} into the following Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP");
	 *      SymbolStruct var3 = var2.findSymbol("+").getSymbol();
	 *      FunctionStructImpl var4 = var3.getFunction();
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link SymbolCompilerFunctionStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<SymbolCompilerFunctionStruct> event) {
		final SymbolCompilerFunctionStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final SymbolStruct functionSymbol = input.getFunctionSymbol();
		final int functionPackageStore = methodBuilder.getNextAvailableStore();
		final int functionSymbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(functionSymbol, generatorState, functionPackageStore, functionSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.SYMBOL_STRUCT_NAME,
		                   GenerationConstants.SYMBOL_STRUCT_GET_FUNCTION_METHOD_NAME,
		                   GenerationConstants.SYMBOL_STRUCT_GET_FUNCTION_METHOD_DESC,
		                   true);
	}
}
