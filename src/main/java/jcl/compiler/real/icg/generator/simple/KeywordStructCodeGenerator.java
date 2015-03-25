/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.KeywordStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class KeywordStructCodeGenerator implements CodeGenerator<KeywordStruct> {

	@Override
	public void generate(final KeywordStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/packages/GlobalPackageStruct", "KEYWORD", "Ljcl/packages/PackageStruct;");
		final int packageStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		final String keywordName = input.getName();

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		mv.visitLdcInsn(keywordName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/symbols/KeywordStruct");
	}
}
