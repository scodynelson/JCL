/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.List;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.real.environment.LetStarEnvironment;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.LetStarStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class LetStarCodeGenerator extends ClosureCreationCodeGenerator<LetStarEnvironment, LetStarStruct.LetStarVar, LetStarStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	private LetStarCodeGenerator() {
		super("letStar");
	}

	@Override
	protected void generateBindings(final List<LetStarStruct.LetStarVar> vars, final GeneratorState generatorState,
	                                final JavaMethodBuilder methodBuilder, final int closureArgStore,
	                                final int newClosureBindingsStore, final Set<Integer> lexicalSymbolStoresToUnbind,
	                                final Set<Integer> dynamicSymbolStoresToUnbind) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();

		for (final LetStarStruct.LetStarVar var : vars) {
			final SymbolStruct<?> symbolVar = var.getVar();
			final LispStruct initForm = var.getInitForm();
			final boolean isSpecial = var.isSpecial();

			final int symbolStore = methodBuilder.getNextAvailableStore();
			// NOTE: we have to get a new 'symbolStore' for each var so we can properly unbind the initForms later

			CodeGenerators.generateSymbol(symbolVar, methodBuilder, packageStore, symbolStore);

			codeGenerator.generate(initForm, generatorState);
			final int initFormStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			CodeGenerators.generateValuesCheckAndStore(methodBuilder, initFormStore);

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
	}
}
