/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
@AllArgsConstructor
public class SymbolCompilerFunctionStruct implements CompilerFunctionStruct {

	private final SymbolStruct functionSymbol;

	/**
	 * {@inheritDoc} Generation method for {@code SymbolCompilerFunctionStruct} objects, by performing the following
	 * operations:
	 * <ol>
	 * <li>Generating the {@link SymbolCompilerFunctionStruct#functionSymbol} value</li>
	 * <li>Generating the code to retrieve the {@link Environment#getFunction(SymbolStruct)} call to retrieve the associated
	 * {@link FunctionStruct} associated with the function symbol</li>
	 * </ol>
	 * As an example, it will transform the function symbol '+' for {@code (+ 1)} into the following Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP");
	 *      SymbolStruct var3 = var2.findSymbol("+").getSymbol();
	 *      FunctionStructImpl var4 = var1.getFunction(var3);
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaEnvironmentMethodBuilder methodBuilder = generatorState.getCurrentEnvironmentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int functionPackageStore = methodBuilder.getNextAvailableStore();
		final int functionSymbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(functionSymbol, generatorState, functionPackageStore, functionSymbolStore);

		final int environmentStore = methodBuilder.getEnvironmentStore();

		mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.ENVIRONMENT_NAME,
		                   GenerationConstants.ENVIRONMENT_GET_FUNCTION_METHOD_NAME,
		                   GenerationConstants.ENVIRONMENT_GET_FUNCTION_METHOD_DESC,
		                   false);
	}

	@Override
	public LispStruct eval(final Environment environment) {
		return environment.getFunction(functionSymbol);
	}
}
