/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class SymbolBindingCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final String packageName = input.getSymbolPackage().getName();
		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
		final int packageStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		final String symbolName = input.getName();
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
		final int symbolStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();
		final Environment currentEnvironment = bindingStack.peek();

		final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(input);
		final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(input);

		if (hasLexicalBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "getLexicalValue", "()Ljcl/LispStruct;", false);
		} else if (hasDynamicBinding) {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "getDynamicValue", "()Ljcl/LispStruct;", false);
		} else {
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "getValue", "()Ljcl/LispStruct;", false);
		}
	}
}