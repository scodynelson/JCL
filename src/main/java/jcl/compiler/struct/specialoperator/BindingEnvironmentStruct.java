/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public abstract class BindingEnvironmentStruct extends CompilerSpecialOperatorStruct {

	protected final List<BindingVar> vars;
	private final PrognStruct forms;
	private final Environment environment;

	protected BindingEnvironmentStruct(final String methodNamePrefix,
	                                   final List<BindingVar> vars, final PrognStruct forms, final Environment environment) {
		super(methodNamePrefix);
		this.vars = vars;
		this.forms = forms;
		this.environment = environment;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link BindingEnvironmentStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the code to create a new {@link Environment}</li>
	 * <li>Calling the implementation of {@link #generateBindings(List, GeneratorState, JavaMethodBuilder, int, int,
	 * Set, Set)} to generate the initial binding of the variables</li>
	 * <li>Initializing a try-catch block</li>
	 * <li>Temporarily pushing the {@link BindingEnvironmentStruct#environment} onto the {@link
	 * GeneratorState#environmentDeque} while generating the code for the {@link BindingEnvironmentStruct#forms} values
	 * inside the try block, ensuring to store the final result into a variable</li>
	 * <li>Generating the code to unbind the variables from {@link SymbolStruct}s as part of the error free
	 * 'finally'</li>
	 * <li>Generating the code to unbind the variables from {@link SymbolStruct}s as part of the error caught
	 * 'finally', ensuring the error caught is re-thrown</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();
		final int environmentStore = methodBuilder.getEnvironmentStore();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.ENVIRONMENT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.ENVIRONMENT_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.ENVIRONMENT_INIT_ENVIRONMENT_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ASTORE, environmentStore);

		final Set<SymbolStruct> existingLexicalSymbols = new HashSet<>(generatorState.getLexicalSymbols());
		final Set<SymbolStruct> existingDynamicSymbols = new HashSet<>(generatorState.getDynamicSymbols());

		for (final BindingVar var : vars) {
			final SymbolStruct symbol = var.getVar();
			if (var.isSpecial()) {
				generatorState.getDynamicSymbols().add(symbol);
			} else {
				generatorState.getLexicalSymbols().add(symbol);
			}
		}

		final Set<Integer> lexicalSymbolStoresToUnbind = new HashSet<>();
		final Set<Integer> dynamicSymbolStoresToUnbind = new HashSet<>();
		generateBindings(generatorState, methodBuilder, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);

		forms.generate(generatorState);

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, environmentStore, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, environmentStore, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		for (final BindingVar var : vars) {
			final SymbolStruct symbol = var.getVar();
			if (var.isSpecial()) {
				if (!existingDynamicSymbols.contains(symbol)) {
					generatorState.getDynamicSymbols().remove(symbol);
				}
			} else {
				if (!existingLexicalSymbols.contains(symbol)) {
					generatorState.getLexicalSymbols().remove(symbol);
				}
			}
		}

		mv.visitInsn(Opcodes.ARETURN);
	}

	/**
	 * Abstract method to perform {@link SymbolStruct} symbol binding generation logic for the provided {@link List}
	 * variables.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param lexicalSymbolStoresToUnbind
	 * 		the {@link Set} of storage location indexes on the stack where the {@link SymbolStruct}s with lexical
	 * 		values to unbind exists
	 * @param dynamicSymbolStoresToUnbind
	 * 		the {@link Set} of storage location indexes on the stack where the {@link SymbolStruct}s with dynamic
	 * 		values to unbind exists
	 */
	protected abstract void generateBindings(GeneratorState generatorState, JavaEnvironmentMethodBuilder methodBuilder,
	                                         Set<Integer> lexicalSymbolStoresToUnbind,
	                                         Set<Integer> dynamicSymbolStoresToUnbind);

	/**
	 * Private method for generating the 'finally' block code for unbinding the lexical and dynamic values from each
	 * {@link SymbolStruct} at the storage location of each of the {@code lexicalSymbolStoresToUnbind} and {@code
	 * dynamicSymbolStoresToUnbind}.
	 *
	 * @param mv
	 * 		the current {@link MethodVisitor} to generate the code inside
	 * @param lexicalSymbolStoresToUnbind
	 * 		the {@link Set} of storage location indexes on the stack where the {@link SymbolStruct}s with lexical
	 * 		values to unbind exists
	 * @param dynamicSymbolStoresToUnbind
	 * 		the {@link Set} of storage location indexes on the stack where the {@link SymbolStruct}s with dynamic
	 * 		values to unbind exists
	 */
	private static void generateFinallyCode(final MethodVisitor mv, final int environmentStore,
	                                        final Set<Integer> lexicalSymbolStoresToUnbind,
	                                        final Set<Integer> dynamicSymbolStoresToUnbind) {
		for (final Integer symbolStore : dynamicSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
			                   false);
		}
		for (final Integer symbolStore : lexicalSymbolStoresToUnbind) {
			mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_UNBIND_LEXICAL_VALUE_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_UNBIND_LEXICAL_VALUE_METHOD_DESC,
			                   false);
		}
	}

	@Getter
	@AllArgsConstructor
	public static class BindingVar {
		private final SymbolStruct var;
		private final LispStruct initForm;
		private final boolean isSpecial;
	}
}
