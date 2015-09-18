/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.ClosureCreationStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.functions.Closure;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Abstract class for performing generation of {@link ClosureCreationStruct}s that create a new {@link Closure} for
 * binding variables and executing form logic within the scope where {@link ClosureCreationStruct#vars} are bound and
 * have effect.
 *
 * @param <E>
 * 		the {@link Environment} type of the {@link ClosureCreationStruct#environment} value
 * @param <V>
 * 		the variable type of the {@link ClosureCreationStruct#vars} values
 * @param <I>
 * 		the type of {@link ClosureCreationStruct} to generate code for
 */
abstract class ClosureCreationCodeGenerator<E extends Environment, V, I extends ClosureCreationStruct<E, V>> extends SpecialOperatorCodeGenerator<I> {

	/**
	 * {@link PrognCodeGenerator} used for generating the {@link ClosureCreationStruct#forms}.
	 */
	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	/**
	 * Protected constructor.
	 *
	 * @param methodNamePrefix
	 * 		the {@link String} to be used as the method name prefix when creating the new method
	 */
	protected ClosureCreationCodeGenerator(final String methodNamePrefix) {
		super(methodNamePrefix);
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ClosureCreationStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the code to create a new {@link Closure}</li>
	 * <li>Generating the code to get the {@link Map} of symbol bindings from the newly created {@link Closure} via
	 * {@link Closure#getSymbolBindings()}</li>
	 * <li>Calling the implementation of {@link #generateBindings(List, GeneratorState, JavaMethodBuilder, int, int,
	 * Set, Set)} to generate the initial binding of the variables</li>
	 * <li>Initializing a try-catch block</li>
	 * <li>Temporarily pushing the {@link ClosureCreationStruct#environment} onto the {@link
	 * GeneratorState#environmentDeque} while generating the code for the {@link ClosureCreationStruct#forms} values
	 * inside the try block, ensuring to store the final result into a variable</li>
	 * <li>Generating the code to unbind the variables from {@link SymbolStruct}s as part of the error free
	 * 'finally'</li>
	 * <li>Generating the code to unbind the variables from {@link SymbolStruct}s as part of the error caught
	 * 'finally', ensuring the error caught is re-thrown</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link I} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final I input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

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
		final int closureSymbolBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingsStore);

		final List<V> vars = input.getVars();
		final Set<Integer> lexicalSymbolStoresToUnbind = new HashSet<>();
		final Set<Integer> dynamicSymbolStoresToUnbind = new HashSet<>();
		generateBindings(vars, generatorState, methodBuilder, closureArgStore, closureSymbolBindingsStore, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);

		final E environment = input.getEnvironment();
		final PrognStruct forms = input.getForms();

		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(environment);
		prognCodeGenerator.generate(forms, generatorState);
		environmentDeque.removeFirst();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, lexicalSymbolStoresToUnbind, dynamicSymbolStoresToUnbind);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);
	}

	/**
	 * Abstract method to perform {@link SymbolStruct} symbol binding generation logic for the provided {@link List<V>}
	 * variables.
	 *
	 * @param vars
	 * 		the {@link List<V>} variables to generate appropriate binding initialization code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 * @param closureSymbolBindingsStore
	 * 		the storage location index on the stack where the {@link Closure#symbolBindings} variable exists
	 * @param lexicalSymbolStoresToUnbind
	 * 		the {@link Set} of storage location indexes on the stack where the {@link SymbolStruct}s with lexical
	 * 		values to unbind exists
	 * @param dynamicSymbolStoresToUnbind
	 * 		the {@link Set} of storage location indexes on the stack where the {@link SymbolStruct}s with dynamic
	 * 		values to unbind exists
	 */
	protected abstract void generateBindings(final List<V> vars, final GeneratorState generatorState,
	                                         final JavaMethodBuilder methodBuilder, final int closureArgStore,
	                                         final int closureSymbolBindingsStore, final Set<Integer> lexicalSymbolStoresToUnbind,
	                                         final Set<Integer> dynamicSymbolStoresToUnbind);

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
	private static void generateFinallyCode(final MethodVisitor mv, final Set<Integer> lexicalSymbolStoresToUnbind,
	                                        final Set<Integer> dynamicSymbolStoresToUnbind) {
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
	}
}
