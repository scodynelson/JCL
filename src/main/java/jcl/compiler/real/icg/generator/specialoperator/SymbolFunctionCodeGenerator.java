/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
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

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final String packageName = functionSymbol.getSymbolPackage().getName();
		final String symbolName = functionSymbol.getName();

		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
		final int packageStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
		final int functionSymbolStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "getFunction", "()Ljcl/functions/FunctionStruct;", false);
	}
}
