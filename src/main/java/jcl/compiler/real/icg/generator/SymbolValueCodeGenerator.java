/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
class SymbolValueCodeGenerator implements CodeGenerator<SymbolStruct<LispStruct>> {

	@Override
	public void generate(final SymbolStruct<LispStruct> input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(input, methodBuilder, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		final Environment currentEnvironment = generatorState.getCurrentEnvironment();

		final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(input);
		final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(input);

		if (hasLexicalBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_GET_LEXICAL_VALUE_METHOD_DESC,
					false);
		} else if (hasDynamicBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_GET_DYNAMIC_VALUE_METHOD_DESC,
					false);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_GET_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_GET_VALUE_METHOD_DESC,
					false);
		}
	}
}
