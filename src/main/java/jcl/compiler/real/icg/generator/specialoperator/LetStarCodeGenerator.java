/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LetEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.LetStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

//@Component
public class LetStarCodeGenerator implements CodeGenerator<LetStruct> {

//	@Autowired
	private FormGenerator formGenerator;

//	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final LetStruct input, final JavaClassBuilder classBuilder) {

		final List<LetStruct.LetVar> vars = input.getVars();
		final PrognStruct forms = input.getForms();
		final LetEnvironment letEnvironment = input.getLetEnvironment();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		final int packageStore = currentClass.getNextAvailableStore();
		final int initFormStore = currentClass.getNextAvailableStore();

		final Set<Integer> lexicalSymbolStoresToUnbind = new HashSet<>();
		final Set<Integer> dynamicSymbolStoresToUnbind = new HashSet<>();

		for (final LetStruct.LetVar var : vars) {
			final SymbolStruct<?> symbolVar = var.getVar();
			final LispStruct initForm = var.getInitForm();
			final boolean isSpecial = var.isSpecial();

			final String packageName = symbolVar.getSymbolPackage().getName();
			final String symbolName = symbolVar.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
			// NOTE: we have to get a new 'symbolStore' for each var so we can properly unbind the initForms later
			final int symbolStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);

			formGenerator.generate(initForm, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

			if (isSpecial) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindDynamicValue", "(Ljcl/LispStruct;)V", false);
				dynamicSymbolStoresToUnbind.add(symbolStore);
			} else {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindLexicalValue", "(Ljcl/LispStruct;)V", false);
				lexicalSymbolStoresToUnbind.add(symbolStore);
			}
		}

		mv.visitLabel(tryBlockStart);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		bindingStack.push(letEnvironment);
		prognCodeGenerator.generate(forms, classBuilder);
		bindingStack.pop();

		final int resultStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);
	}

	private void generateFinallyCode(final MethodVisitor mv, final Set<Integer> lexicalSymbolStoresToUnbind,
	                                 final Set<Integer> dynamicSymbolStoresToUnbind) {
		for (final Integer symbolStore : lexicalSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindLexicalValue", "()V", false);
		}

		for (final Integer var : dynamicSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, var);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindDynamicValue", "()V", false);
		}
	}
}
