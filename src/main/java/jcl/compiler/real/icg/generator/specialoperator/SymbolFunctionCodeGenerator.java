/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.simple.SymbolCodeGeneratorUtil;
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class SymbolFunctionCodeGenerator implements CodeGenerator<SymbolCompilerFunctionStruct> {

	@Override
	public void generate(final SymbolCompilerFunctionStruct input, final JavaClassBuilder classBuilder) {

		final SymbolStruct<?> functionSymbol = input.getFunctionSymbol();

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int functionPackageStore = methodBuilder.getNextAvailableStore();
		final int functionSymbolStore = methodBuilder.getNextAvailableStore();
		SymbolCodeGeneratorUtil.generate(functionSymbol, classBuilder, functionPackageStore, functionSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_GET_FUNCTION_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_GET_FUNCTION_METHOD_DESC,
				false);
	}
}
