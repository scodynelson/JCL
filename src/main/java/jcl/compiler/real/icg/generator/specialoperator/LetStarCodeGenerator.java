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
import jcl.compiler.real.environment.LetStarEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.simple.SymbolCodeGeneratorUtil;
import jcl.compiler.real.struct.specialoperator.LetStarStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LetStarCodeGenerator implements CodeGenerator<LetStarStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private static final String LET_STAR_METHOD_NAME_PREFIX = "letStar_";

	private static final String LET_STAR_METHOD_DESC = "(Ljcl/functions/Closure;)Ljcl/LispStruct;";

	@Override
	public void generate(final LetStarStruct input, final JavaClassBuilder classBuilder) {

		final List<LetStarStruct.LetStarVar> vars = input.getVars();
		final PrognStruct forms = input.getForms();
		final LetStarEnvironment letStarEnvironment = input.getLetStarEnvironment();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String letStarMethodName = LET_STAR_METHOD_NAME_PREFIX + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, letStarMethodName, LET_STAR_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = classBuilder.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		final Label finallyBlockStart = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);
		mv.visitTryCatchBlock(catchBlockStart, finallyBlockStart, catchBlockStart, null);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.CLOSURE_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.CLOSURE_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.CLOSURE_INIT_CLOSURE_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, closureArgStore);

		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.CLOSURE_NAME,
				GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME,
				GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_DESC,
				false);
		final int newClosureBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, newClosureBindingsStore);

		final int packageStore = methodBuilder.getNextAvailableStore();

		final Set<Integer> lexicalSymbolStoresToUnbind = new HashSet<>();
		final Set<Integer> dynamicSymbolStoresToUnbind = new HashSet<>();

		for (final LetStarStruct.LetStarVar var : vars) {
			final SymbolStruct<?> symbolVar = var.getVar();
			final LispStruct initForm = var.getInitForm();
			final boolean isSpecial = var.isSpecial();

			final int symbolStore = methodBuilder.getNextAvailableStore();
			// NOTE: we have to get a new 'symbolStore' for each var so we can properly unbind the initForms later

			SymbolCodeGeneratorUtil.generate(symbolVar, classBuilder, packageStore, symbolStore);

			formGenerator.generate(initForm, classBuilder);
			final int initFormStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			final Label valuesCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
			mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
			final int valuesStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

			mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.VALUES_STRUCT_NAME,
					GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME,
					GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			mv.visitLabel(valuesCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

			if (isSpecial) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						GenerationConstants.SYMBOL_STRUCT_NAME,
						GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME,
						GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC,
						false);

				dynamicSymbolStoresToUnbind.add(symbolStore);
			} else {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						GenerationConstants.SYMBOL_STRUCT_NAME,
						GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME,
						GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_DESC,
						false);

				lexicalSymbolStoresToUnbind.add(symbolStore);

				mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
						GenerationConstants.CLOSURE_NAME,
						GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME,
						GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_DESC,
						false);

				mv.visitVarInsn(Opcodes.ALOAD, newClosureBindingsStore);
				mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
				mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
						GenerationConstants.JAVA_MAP_NAME,
						GenerationConstants.JAVA_MAP_PUT_METHOD_NAME,
						GenerationConstants.JAVA_MAP_PUT_METHOD_DESC,
						true);
				mv.visitInsn(Opcodes.POP);
			}
		}

		mv.visitLabel(tryBlockStart);

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		bindingStack.push(letStarEnvironment);
		prognCodeGenerator.generate(forms, classBuilder);
		bindingStack.pop();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		for (final Integer symbolStore : dynamicSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
					false);
		}
		for (final Integer symbolStore : lexicalSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
					false);
		}
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		mv.visitLabel(finallyBlockStart);
		for (final Integer symbolStore : dynamicSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
					false);
		}
		for (final Integer symbolStore : lexicalSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
					false);
		}

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, letStarMethodName, LET_STAR_METHOD_DESC, false);
	}
}
