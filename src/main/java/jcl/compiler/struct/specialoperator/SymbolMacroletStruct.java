/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class SymbolMacroletStruct extends CompilerSpecialOperatorStruct {

	private final List<SymbolMacroletVar> vars;
	private final PrognStruct forms;
	private final Environment symbolMacroletEnvironment;

	public SymbolMacroletStruct(final List<SymbolMacroletVar> vars, final PrognStruct forms, final Environment symbolMacroletEnvironment) {
		super("symbolMacrolet");
		this.vars = vars;
		this.forms = forms;
		this.symbolMacroletEnvironment = symbolMacroletEnvironment;
	}

	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaEnvironmentMethodBuilder methodBuilder = generatorState.getCurrentEnvironmentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Set<SymbolStruct> existingLexicalSymbols = new HashSet<>(generatorState.getLexicalSymbols());

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		final Set<Integer> symbolVarStores = new HashSet<>(vars.size());

		final int environmentStore = methodBuilder.getEnvironmentStore();
		final int packageStore = methodBuilder.getNextAvailableStore();
		final int expansionStore = methodBuilder.getNextAvailableStore();

		for (final SymbolMacroletStruct.SymbolMacroletVar var : vars) {
			final SymbolStruct symbolVar = var.getVar();
			// NOTE: we have to get a new 'symbolStore' for each var so we can properly unbind the expansions later
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(symbolVar, generatorState, packageStore, symbolStore);

			// Add the symbolStore here so we can unbind the expansions later
			symbolVarStores.add(symbolStore);

			final LispStruct expansion = var.getExpansion();
			final String symbolMacroExpanderClassName = generateSymbolMacroExpander(expansion, generatorState);

			mv.visitTypeInsn(Opcodes.NEW, symbolMacroExpanderClassName);
			mv.visitInsn(Opcodes.DUP);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, symbolMacroExpanderClassName, "<init>", "()V", false);
			mv.visitVarInsn(Opcodes.ASTORE, expansionStore);

			mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, expansionStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_BIND_SYMBOL_MACRO_EXPANDER_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_BIND_SYMBOL_MACRO_EXPANDER_METHOD_DESC,
			                   false);

			generatorState.getLexicalSymbols().add(symbolVar);
		}

		mv.visitLabel(tryBlockStart);

		forms.generate(generatorState);

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, environmentStore, symbolVarStores);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, environmentStore, symbolVarStores);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		for (final SymbolMacroletStruct.SymbolMacroletVar var : vars) {
			final SymbolStruct symbol = var.getVar();
			if (!existingLexicalSymbols.contains(symbol)) {
				generatorState.getLexicalSymbols().remove(symbol);
			}
		}
	}

	private static void generateFinallyCode(final MethodVisitor mv, final int environmentStore,
	                                        final Set<Integer> varSymbolStores) {
		for (final Integer varSymbolStore : varSymbolStores) {
			mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
			mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_UNBIND_SYMBOL_MACRO_EXPANDER_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_UNBIND_SYMBOL_MACRO_EXPANDER_METHOD_DESC,
			                   false);
		}
	}

	private static String generateSymbolMacroExpander(final LispStruct expansion, final GeneratorState classBuilder) {

		final String fileName = "SymbolMacrolet" + '_' + System.nanoTime();
		final String className = "jcl/" + fileName;

		final JavaClassBuilder currentClass = new JavaClassBuilder(className, fileName);
		final Deque<JavaClassBuilder> classBuilderDeque = classBuilder.getClassBuilderDeque();

		classBuilderDeque.addFirst(currentClass);
		classBuilder.getFinalClassBuilderDeque().addFirst(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();
		cw.visit(GenerationConstants.JAVA_VERSION, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, className, null, "jcl/compiler/function/expander/SymbolMacroExpander", null);

		cw.visitSource(fileName + ".java", null);

		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Deque<JavaMethodBuilder> methodBuilderDeque = classBuilder.getMethodBuilderDeque();
			methodBuilderDeque.addFirst(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/function/expander/SymbolMacroExpander", "<init>", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderDeque.removeFirst();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "expand", "(Ljcl/lang/LispStruct;Ljcl/compiler/environment/Environment;)Ljcl/lang/LispStruct;", null, null);

			final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
			final Deque<JavaMethodBuilder> methodBuilderDeque = classBuilder.getMethodBuilderDeque();
			methodBuilderDeque.addFirst(methodBuilder);

			mv.visitCode();
			final int thisStore = methodBuilder.getNextAvailableStore();
			final int expressionStore = methodBuilder.getNextAvailableStore();
			final int environmentStore = methodBuilder.getNextAvailableStore();

			expansion.generate(classBuilder);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();

			methodBuilderDeque.removeFirst();
		}
		cw.visitEnd();

		classBuilderDeque.removeFirst();

		return className;
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {
		// Do Nothing.
	}

	@Getter
	@AllArgsConstructor
	public static class SymbolMacroletVar {
		private final SymbolStruct var;
		private final LispStruct expansion;
	}
}
