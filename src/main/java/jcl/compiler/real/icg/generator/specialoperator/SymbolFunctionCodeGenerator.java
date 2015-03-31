/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Map;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.FletEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
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

		final Environment currentEnvironment = classBuilder.getBindingEnvironment();
		if (currentEnvironment instanceof FletEnvironment) {
			final FletEnvironment fletEnvironment = (FletEnvironment) currentEnvironment;
			fletGenerate(currentClass, mv, classBuilder, fletEnvironment, functionSymbol, functionSymbolStore);
		} else {
			nonFletGenerate(mv, functionSymbolStore);
		}
	}

	private void nonFletGenerate(final MethodVisitor mv, final int functionSymbolStore) {

		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "getFunction", "()Ljcl/functions/FunctionStruct;", false);
	}

	private void fletGenerate(final ClassDef currentClass, final MethodVisitor mv, final JavaClassBuilder classBuilder,
	                          final FletEnvironment fletEnvironment, final SymbolStruct<?> functionSymbol,
	                          final int functionSymbolStore) {

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();

		final boolean hasFunctionBinding = fletEnvironment.hasFunctionBinding(functionSymbol);
		if (hasFunctionBinding) {

			final Map<SymbolStruct<?>, Integer> fletFunctionStoresToBind = classBuilder.getFletFunctionStoresToBind();
			if (fletFunctionStoresToBind.containsKey(functionSymbol)) {
				mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

				final Integer initFormStore = fletFunctionStoresToBind.get(functionSymbol);

				mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "bindFunction", "(Ljcl/functions/FunctionStruct;)V", false);

				mv.visitLabel(tryBlockStart);
			}
		}

		nonFletGenerate(mv, functionSymbolStore);

		if (hasFunctionBinding) {

			final Map<SymbolStruct<?>, Integer> fletFunctionStoresToBind = classBuilder.getFletFunctionStoresToBind();
			if (fletFunctionStoresToBind.containsKey(functionSymbol)) {
				final int resultStore = currentClass.getNextAvailableStore();
				mv.visitVarInsn(Opcodes.ASTORE, resultStore);

				mv.visitLabel(tryBlockEnd);
				generateFinallyCode(mv, functionSymbolStore);
				mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

				mv.visitLabel(catchBlockStart);
				final int exceptionStore = currentClass.getNextAvailableStore();
				mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

				generateFinallyCode(mv, functionSymbolStore);

				mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
				mv.visitInsn(Opcodes.ATHROW);

				mv.visitLabel(catchBlockEnd);
				mv.visitVarInsn(Opcodes.ALOAD, resultStore);
			}
		}
	}

	private void generateFinallyCode(final MethodVisitor mv, final Integer functionSymbolStore) {
		mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "unbindFunction", "()V", false);
	}
}
