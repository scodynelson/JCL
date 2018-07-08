/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.FunctionStruct;
import jcl.lang.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SymbolCompilerFunctionStruct implements CompilerFunctionStruct {

	private final SymbolStruct functionSymbol;

	public SymbolCompilerFunctionStruct(final SymbolStruct functionSymbol) {
		this.functionSymbol = functionSymbol;
	}

	public SymbolStruct getFunctionSymbol() {
		return functionSymbol;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link SymbolCompilerFunctionStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link SymbolCompilerFunctionStruct#functionSymbol} value</li>
	 * <li>Generating the code to retrieve the {@link SymbolStruct#getFunction()} call to retrieve the associated
	 * {@link FunctionStruct} associated with the function symbol</li>
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
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

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
