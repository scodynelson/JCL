/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Class to perform 'flet' and 'labels' special operator code generation. Both special operators generate the same
 * code, but will act differently due to differences in the structure of the generated lambda forms as altered in the
 * Semantic Analyzer.
 */
public class InnerLambdaStruct extends CompilerSpecialOperatorStruct {

	private final List<InnerLambdaVar> vars;

	private final PrognStruct forms;

	private final Environment lexicalEnvironment;

	public InnerLambdaStruct(final List<InnerLambdaVar> vars, final PrognStruct forms, final Environment lexicalEnvironment) {
		super("innerLambda");
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<InnerLambdaVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link InnerLambdaStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving the {@link List} of function bindings from the {@link Closure} parameter if the parameter is not
	 * null</li>
	 * <li>Generating each of the {@link InnerLambdaStruct.InnerLambdaVar#var} and {@link
	 * InnerLambdaStruct.InnerLambdaVar#initForm} values</li>
	 * <li>Collect all generated function and form stack locations for lazily binding the functions to the {@link
	 * SymbolStruct}s</li>
	 * <li>Binding functions via {@link SymbolStruct#bindFunction(FunctionStruct)} and adding functions to the {@link
	 * Closure#functionBindings} map for the current {@link Closure}, if one exists</li>
	 * <li>Temporarily pushing the {@link InnerLambdaStruct#lexicalEnvironment} onto the {@link
	 * GeneratorState#environmentDeque} while generating the code for the {@link InnerLambdaStruct#forms} values</li>
	 * <li>Generating the code to unbind the functions from {@link SymbolStruct}s as part of the error free
	 * 'finally'</li>
	 * <li>Generating the code to unbind the functions from {@link SymbolStruct}s as part of the error caught
	 * 'finally', ensuring the error caught is re-thrown</li>
	 * </ol>
	 * As an example, it will transform {@code (flet ((foo () 1)) (foo))} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct innerLambda_1(Closure var1) {
	 *      Map var2 = null;
	 *      if(var1 != null) {
	 *          var2 = var1.getFunctionBindings();
	 *      }
	 *
	 *      PackageStruct var3 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var4 = var3.findSymbol("FOO").getSymbol();
	 *      FLET_FOO_Lambda_123456789 var5 = new FLET_FOO_Lambda_123456789(var1);
	 *      var4.bindFunction(var5);
	 *      if(var2 != null) {
	 *          var2.put(var4, var5);
	 *      }
	 *
	 *      LispStruct var11;
	 *      try {
	 *          PackageStruct var6 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var7 = var6.findSymbol("FOO").getSymbol();
	 *          FunctionStructImpl var8 = var7.getFunction();
	 *          LispStruct[] var9 = new LispStruct[0];
	 *          var11 = var8.apply(var9);
	 *      } finally {
	 *          var4.unbindFunction();
	 *      }
	 *
	 *      return var11;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitInsn(Opcodes.ACONST_NULL);
		final int closureFunctionBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

		final Label closureNullCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		mv.visitJumpInsn(Opcodes.IFNULL, closureNullCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.CLOSURE_NAME,
		                   GenerationConstants.CLOSURE_GET_FUNCTION_BINDINGS_METHOD_NAME,
		                   GenerationConstants.CLOSURE_GET_FUNCTION_BINDINGS_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ASTORE, closureFunctionBindingsStore);

		mv.visitLabel(closureNullCheckIfEnd);

		final int packageStore = methodBuilder.getNextAvailableStore();

		final Map<Integer, Integer> functionStoresToBind = new HashMap<>();

		for (final InnerLambdaStruct.InnerLambdaVar var : vars) {
			final SymbolStruct functionSymbolVar = var.getVar();
			final int functionSymbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(functionSymbolVar, generatorState, packageStore, functionSymbolStore);

			final CompilerFunctionStruct initForm = var.getInitForm();
			initForm.generate(generatorState);

			final int initFormStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			functionStoresToBind.put(functionSymbolStore, initFormStore);
		}

		for (final Map.Entry<Integer, Integer> functionStoreToBind : functionStoresToBind.entrySet()) {
			final Integer functionSymbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_BIND_FUNCTION_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_BIND_FUNCTION_METHOD_DESC,
			                   true);

			final Label closureFunctionBindingsNullCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			mv.visitJumpInsn(Opcodes.IFNULL, closureFunctionBindingsNullCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureFunctionBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_MAP_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(closureFunctionBindingsNullCheckIfEnd);
		}

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, null);

		mv.visitLabel(tryBlockStart);

		final Deque<Environment> environmentDeque = generatorState.getEnvironmentDeque();

		environmentDeque.addFirst(lexicalEnvironment);
		forms.generate(generatorState);
		environmentDeque.removeFirst();

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		final Set<Integer> functionSymbolStores = functionStoresToBind.keySet();

		mv.visitLabel(tryBlockEnd);
		generateFinallyCode(mv, functionSymbolStores);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int exceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, exceptionStore);

		generateFinallyCode(mv, functionSymbolStores);

		mv.visitVarInsn(Opcodes.ALOAD, exceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);
	}

	/**
	 * Private method for generating the 'finally' block code for unbinding the function values from each {@link
	 * SymbolStruct} at the storage location of each of the {@code functionSymbolStores}.
	 *
	 * @param mv
	 * 		the current {@link MethodVisitor} to generate the code inside
	 * @param functionSymbolStores
	 * 		the {@link Set} of storage location indexes on the stack where the {@link SymbolStruct}s to unbind function
	 * 		values from exist
	 */
	private static void generateFinallyCode(final MethodVisitor mv, final Set<Integer> functionSymbolStores) {
		for (final Integer functionSymbolStore : functionSymbolStores) {
			mv.visitVarInsn(Opcodes.ALOAD, functionSymbolStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_UNBIND_FUNCTION_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);
		}
	}

	public static class InnerLambdaVar {

		private final SymbolStruct var;

		private final CompilerFunctionStruct initForm;

		private final boolean isSpecial;

		public InnerLambdaVar(final SymbolStruct var, final CompilerFunctionStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct getVar() {
			return var;
		}

		public CompilerFunctionStruct getInitForm() {
			return initForm;
		}

		public boolean isSpecial() {
			return isSpecial;
		}
	}
}
