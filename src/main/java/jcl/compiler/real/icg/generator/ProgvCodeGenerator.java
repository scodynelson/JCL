/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.util.Deque;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.Closure;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'progv' special operator code generation.
 */
@Component
final class ProgvCodeGenerator extends SpecialOperatorCodeGenerator<ProgvStruct> {

	/**
	 * Constant {@link String} containing the error message prefix for the customized {@link ProgramErrorException}
	 * thrown if the {@link ProgvStruct#vars} value does not produce a {@link ListStruct} value.
	 */
	private static final String SYMBOLS_LIST_MUST_BE_A_LIST = "PROGV: Symbols list must be a list. Got: ";

	/**
	 * Constant {@link String} containing the error message prefix for the customized {@link ProgramErrorException}
	 * thrown if each of the elements comprising the {@link ProgvStruct#vars} {@link ListStruct} value does not produce
	 * a {@link SymbolStruct} value.
	 */
	private static final String ELEMENTS_IN_SYMBOLS_LIST_MUST_BE_SYMBOLS = "PROGV: Elements in symbols list must be symbols. Got: ";

	/**
	 * Constant {@link String} containing the error message prefix for the customized {@link ProgramErrorException}
	 * thrown if the {@link ProgvStruct#vals} value does not produce a {@link ListStruct} value.
	 */
	private static final String VALUES_LIST_MUST_BE_A_LIST = "PROGV: Values list must be a list. Got: ";

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link ProgvStruct#vars} and {@link ProgvStruct#vals}
	 * values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@link PrognCodeGenerator} used for generating the {@link ProgvStruct#forms}.
	 */
	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	/**
	 * Private constructor which passes 'progv' as the prefix value to be set in it's {@link #methodNamePrefix} value.
	 */
	private ProgvCodeGenerator() {
		super("progv");
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ProgvStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link ProgvStruct#vars} value, checking that the var result is a {@link ListStruct} and that
	 * each of the vars are {@link SymbolStruct}s</li>
	 * <li>Generating the {@link ProgvStruct#vals} value, checking that the val result is a {@link ListStruct}</li>
	 * <li>Binding the vals comprising the {@link ListStruct} to the vars comprising the other {@link ListStruct} in
	 * sequential, matching order via {@link SymbolStruct#bindDynamicValue(LispStruct)}</li>
	 * <li>Temporarily pushing the {@link ProgvStruct#progvEnvironment} onto the {@link
	 * GeneratorState#environmentDeque} while generating the code for the {@link ProgvStruct#forms} values</li>
	 * <li>Generating the code to unbind the vals dynamic binding from {@link SymbolStruct}s as part of the error free
	 * 'finally'</li>
	 * <li>Generating the code to unbind the vals dynamic binding from {@link SymbolStruct}s as part of the error
	 * caught 'finally', ensuring the error caught is re-thrown</li>
	 * </ol>
	 * As an example, it will transform {@code (progv '(x) '(1) x)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct progv_1(Closure var1) {
	 *      LispStruct var2 = this.symbolFunctionCall_1(var1);
	 *      if(!(var2 instanceof ListStruct)) {
	 *          throw new ProgramErrorException("PROGV: Symbols list must be a list. Got: " + var2);
	 *      }
	 *
	 *      ListStruct var3 = (ListStruct)var2;
	 *      List var4 = var3.getAsJavaList();
	 *      Iterator var5 = var4.iterator();
	 *
	 *      while(var5.hasNext()) {
	 *          LispStruct var6 = (LispStruct)var5.next();
	 *          if(!(var6 instanceof SymbolStruct)) {
	 *              throw new ProgramErrorException("PROGV: Elements in symbols list must be symbols. Got: " + var6);
	 *          }
	 *      }
	 *
	 *      LispStruct var7 = this.symbolFunctionCall_2(var1);
	 *      if(!(var7 instanceof ListStruct)) {
	 *          throw new ProgramErrorException("PROGV: Values list must be a list. Got: " + var7);
	 *      }
	 *
	 *      ListStruct var8 = (ListStruct)var7;
	 *      List var9 = var8.getAsJavaList();
	 *      int var10 = var4.size();
	 *      int var11 = var9.size();
	 *
	 *      for(int var12 = 0; var12 < var10; ++var12) {
	 *          SymbolStruct var13 = (SymbolStruct)var4.get(var12);
	 *          LispStruct var14 = null;
	 *          if(var12 < var11) {
	 *              var14 = (LispStruct)var9.get(var12);
	 *          }
	 *
	 *          if(var14 instanceof ValuesStruct) {
	 *              ValuesStruct var15 = (ValuesStruct)var14;
	 *              var14 = var15.getPrimaryValue();
	 *          }
	 *
	 *          var13.bindDynamicValue(var14);
	 *      }
	 *
	 *      LispStruct var18;
	 *      try {
	 *          PackageStruct var16 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var17 = var16.findSymbol("X").getSymbol();
	 *          var18 = var17.getValue();
	 *      } finally {
	 *          Iterator var22 = var4.iterator();
	 *
	 *          while(var22.hasNext()) {
	 *              SymbolStruct var23 = (SymbolStruct)var22.next();
	 *              var23.unbindDynamicValue();
	 *          }
	 *      }
	 *      return var18;
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link ProgvStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final ProgvStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		// Generate and Check Vars List
		final LispStruct vars = input.getVars();
		final int varsJavaListStore = generateListAndCheck(vars, generatorState, methodBuilder, SYMBOLS_LIST_MUST_BE_A_LIST);
		generateVarListSymbolsCheck(methodBuilder, varsJavaListStore);

		// Generate and Check Vals List
		final LispStruct vals = input.getVals();
		final int valsJavaListStore = generateListAndCheck(vals, generatorState, methodBuilder, VALUES_LIST_MUST_BE_A_LIST);

		// Generate Dynamic Var Binding Loop
		generateVarBindingLoop(methodBuilder, varsJavaListStore, valsJavaListStore);

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);

		final ProgvEnvironment environment = input.getProgvEnvironment();
		final PrognStruct forms = input.getForms();

		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(environment);
		prognCodeGenerator.generate(forms, generatorState);
		environmentDeque.removeFirst();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(methodBuilder, varsJavaListStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(methodBuilder, varsJavaListStore);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);
	}

	/**
	 * Private method to handle the generation of a {@link LispStruct} that could possibly be a {@link ListStruct}. If
	 * it does evaluate to a {@link ListStruct} at runtime, the code to generate the return of a {@link List} via
	 * {@link ListStruct#getAsJavaList()} is executed. Otherwise, the code to generate a {@link ProgramErrorException}
	 * and throw is executed.
	 *
	 * @param possibleList
	 * 		the {@link LispStruct} to be generated that might be a possible {@link ListStruct}
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param mustBeListErrorString
	 * 		the {@link String} to use as the error message when creating the {@link ProgramErrorException} when the
	 * 		provided {@code possibleList} does not produce a {@link ListStruct}
	 *
	 * @return the storage location index on the stack where the {@link List} produced from generating the possible
	 * {@link ListStruct} and calling its {@link ListStruct#getAsJavaList()} method produces
	 */
	private int generateListAndCheck(final LispStruct possibleList, final GeneratorState generatorState,
	                                 final JavaMethodBuilder methodBuilder, final String mustBeListErrorString) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		codeGenerator.generate(possibleList, generatorState);

		final int inputStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, inputStore);

		final Label inputListCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, inputStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.LIST_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, inputListCheckEnd);

		generateProgramError(mv, inputStore, mustBeListErrorString);

		mv.visitLabel(inputListCheckEnd);

		mv.visitVarInsn(Opcodes.ALOAD, inputStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LIST_STRUCT_NAME);
		final int inputListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, inputListStore);

		mv.visitVarInsn(Opcodes.ALOAD, inputListStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.LIST_STRUCT_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_NAME,
				GenerationConstants.LIST_STRUCT_GET_AS_JAVA_LIST_METHOD_DESC,
				false);
		final int inputJavaListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, inputJavaListStore);

		return inputJavaListStore;
	}

	/**
	 * Private method to handle the generation of a type check against all the elements of the {@link List} at the
	 * provided {@code varsJavaListStore} to ensure each of the elements is a {@link SymbolStruct}. If any of them are
	 * not, the generated {@link ProgramErrorException} response is thrown.
	 *
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param varsJavaListStore
	 * 		the storage location index on the stack where the {@link List} of {@link SymbolStruct} vars exist
	 */
	private static void generateVarListSymbolsCheck(final JavaMethodBuilder methodBuilder, final int varsJavaListStore) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int varsJavaListIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varsJavaListIteratorStore);

		final Label varsListOfSymbolsIteratorLoopStart = new Label();
		final Label varsListOfSymbolsIteratorLoopEnd = new Label();

		mv.visitLabel(varsListOfSymbolsIteratorLoopStart);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);
		mv.visitJumpInsn(Opcodes.IFEQ, varsListOfSymbolsIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
		final int varSymbolCheckStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varSymbolCheckStore);

		final Label varSymbolCheckEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, varSymbolCheckStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.SYMBOL_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, varSymbolCheckEnd);

		generateProgramError(mv, varSymbolCheckStore, ELEMENTS_IN_SYMBOLS_LIST_MUST_BE_SYMBOLS);

		mv.visitLabel(varSymbolCheckEnd);

		mv.visitJumpInsn(Opcodes.GOTO, varsListOfSymbolsIteratorLoopStart);

		mv.visitLabel(varsListOfSymbolsIteratorLoopEnd);
	}

	/**
	 * Private method to handle the generation of dynamic binding of the {@link List} of vars at the {@code
	 * varsJavaListStore} location and the {@link List} of vals at the {@code valsJavaListStore} location. The var
	 * {@link SymbolStruct}s are bound via {@link SymbolStruct#bindDynamicValue(LispStruct)} by looping through the
	 * lists binding the vars to the matching vals sequentially and to 'null' (unbound) if the {@link List} of vals is
	 * greater than the list of vars.
	 *
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param varsJavaListStore
	 * 		the storage location index on the stack where the {@link List} of {@link SymbolStruct} vars exist
	 * @param valsJavaListStore
	 * 		the storage location index on the stack where the {@link List} of {@link LispStruct} vals exist
	 */
	private static void generateVarBindingLoop(final JavaMethodBuilder methodBuilder, final int varsJavaListStore,
	                                           final int valsJavaListStore) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_DESC,
				true);
		final int varsJavaListSizeStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, varsJavaListSizeStore);

		mv.visitVarInsn(Opcodes.ALOAD, valsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_NAME,
				GenerationConstants.JAVA_LIST_SIZE_METHOD_DESC,
				true);
		final int valsJavaListSizeStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, valsJavaListSizeStore);

		mv.visitInsn(Opcodes.ICONST_0);
		final int indexValueStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, indexValueStore);

		final Label varBindingLoopStart = new Label();
		final Label varBindingLoopEnd = new Label();

		mv.visitLabel(varBindingLoopStart);

		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitVarInsn(Opcodes.ILOAD, varsJavaListSizeStore);
		mv.visitJumpInsn(Opcodes.IF_ICMPGE, varBindingLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int varSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, varSymbolStore);

		mv.visitInsn(Opcodes.ACONST_NULL);
		final int valStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitVarInsn(Opcodes.ILOAD, valsJavaListSizeStore);

		final Label numberOfValsCheckEnd = new Label();

		mv.visitJumpInsn(Opcodes.IF_ICMPGE, numberOfValsCheckEnd);
		mv.visitVarInsn(Opcodes.ALOAD, valsJavaListStore);
		mv.visitVarInsn(Opcodes.ILOAD, indexValueStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_NAME,
				GenerationConstants.JAVA_LIST_GET_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_NAME);
		mv.visitVarInsn(Opcodes.ASTORE, valStore);

		mv.visitLabel(numberOfValsCheckEnd);

		CodeGenerators.generateValuesCheckAndStore(methodBuilder, valStore);

		mv.visitVarInsn(Opcodes.ALOAD, varSymbolStore);
		mv.visitVarInsn(Opcodes.ALOAD, valStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC,
				false);

		mv.visitIincInsn(indexValueStore, 1);
		mv.visitJumpInsn(Opcodes.GOTO, varBindingLoopStart);

		mv.visitLabel(varBindingLoopEnd);
	}

	/**
	 * Private method for generating the 'finally' block code for unbinding the dynamic {@link ProgvStruct#vars} from
	 * each {@link SymbolStruct} within the {@link List} at the storage location of the {@code varJavaListStore}.
	 *
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param varsJavaListStore
	 * 		the storage location index on the stack where the {@link List} of {@link SymbolStruct}s to unbind dynamic
	 * 		values from exists
	 */
	private static void generateFinallyCode(final JavaMethodBuilder methodBuilder, final int varsJavaListStore) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitVarInsn(Opcodes.ALOAD, varsJavaListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_LIST_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_NAME,
				GenerationConstants.JAVA_LIST_ITERATOR_METHOD_DESC,
				true);
		final int unbindingIteratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, unbindingIteratorStore);

		final Label unbindingIteratorLoopStart = new Label();
		final Label unbindingIteratorLoopEnd = new Label();

		mv.visitLabel(unbindingIteratorLoopStart);
		mv.visitVarInsn(Opcodes.ALOAD, unbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_HAS_NEXT_METHOD_DESC,
				true);
		mv.visitJumpInsn(Opcodes.IFEQ, unbindingIteratorLoopEnd);

		mv.visitVarInsn(Opcodes.ALOAD, unbindingIteratorStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				GenerationConstants.JAVA_ITERATOR_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_NAME,
				GenerationConstants.JAVA_ITERATOR_NEXT_METHOD_DESC,
				true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.SYMBOL_STRUCT_NAME);
		final int unbindingVarSymbolStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, unbindingVarSymbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, unbindingVarSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_NAME,
				GenerationConstants.SYMBOL_STRUCT_UNBIND_DYNAMIC_VALUE_METHOD_DESC,
				false);

		mv.visitJumpInsn(Opcodes.GOTO, unbindingIteratorLoopStart);

		mv.visitLabel(unbindingIteratorLoopEnd);
	}

	/**
	 * Private method for generating the code to create and throw a customized {@link ProgramErrorException} when the
	 * {@link ProgvStruct#vars} value does not generate a {@link ListStruct} value, the {@link ProgvStruct#vals} value
	 * does not produce a {@link ListStruct} value, or each of the elements comprising the {@link ProgvStruct#vars}
	 * {@link ListStruct} value does not produce a {@link SymbolStruct} value.
	 *
	 * @param mv
	 * 		the current {@link MethodVisitor} to generate the code inside
	 * @param itemStore
	 * 		the storage location index on the stack where the generated item exists
	 * @param errorString
	 * 		the {@link String} to use as the error message when creating the {@link ProgramErrorException}
	 */
	private static void generateProgramError(final MethodVisitor mv, final int itemStore, final String errorString) {
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_STRING_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_INIT_DESC,
				false);
		mv.visitLdcInsn(errorString);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_STRING_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ALOAD, itemStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_APPEND_OBJECT_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.JAVA_STRING_BUILDER_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME,
				GenerationConstants.JAVA_STRING_BUILDER_TO_STRING_METHOD_DESC,
				false);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);
	}
}
