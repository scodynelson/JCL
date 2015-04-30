/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class SymbolBindingCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String packageName = input.getSymbolPackage().getName();
		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
				GenerationConstants.PACKAGE_STRUCT_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC,
				false);
		final int packageStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		final String symbolName = input.getName();
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.PACKAGE_STRUCT_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_SYMBOL_METHOD_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_SYMBOL_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_NAME,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC,
				false);
		final int symbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();
		final Environment currentEnvironment = bindingStack.peek();

		final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(input);
		final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(input);

		if (hasLexicalBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, GenerationConstants.SYMBOL_STRUCT_NAME, "getLexicalValue", "()Ljcl/LispStruct;", false);
		} else if (hasDynamicBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, GenerationConstants.SYMBOL_STRUCT_NAME, "getDynamicValue", "()Ljcl/LispStruct;", false);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, GenerationConstants.SYMBOL_STRUCT_NAME, "getValue", "()Ljcl/LispStruct;", false);
		}
	}
}
