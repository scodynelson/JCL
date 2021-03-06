/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Class to perform 'let' special operator code generation. 'Let' is different from 'Let*' in that the binding of the
 * {@link SymbolStruct}s to their scoped {@link LispStruct} values occurs after all the forms have had their code
 * generated. This ensures that no variables are affected nor dependent on the definition of others.
 */
public class LetStruct extends ClosureCreationStruct<LetStruct.LetVar> {

	public LetStruct(final List<LetVar> vars, final PrognStruct forms, final Environment letEnvironment) {
		super("let", vars, forms, letEnvironment);
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LetStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating each of the {@link LetStruct.LetVar#var} and {@link LetStruct.LetVar#initForm} values</li>
	 * <li>Collect all generated symbol and form stack locations for lazily binding the values to the {@link
	 * SymbolStruct}s</li>
	 * <li>Symbol bindings where {@link LetStruct.LetVar#isSpecial} is true are binded via {@link
	 * SymbolStruct#bindDynamicValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link LetStruct.LetVar#isSpecial} is false are binded via {@link
	 * SymbolStruct#bindLexicalValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link LetStruct.LetVar#isSpecial} is false are also added to the {@link
	 * Closure#symbolBindings} map for the new {@link Closure} created with the 'let'</li>
	 * </ol>
	 * As an example, it will transform {@code (let ((x 1)) x)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct let_1(Closure var1) {
	 *      var1 = new Closure(var1);
	 *      Map var2 = var1.getSymbolBindings();
	 *
	 *      PackageStruct var3 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var4 = var3.findSymbol("X").getSymbol();
	 *
	 *      BigInteger var5 = new BigInteger("1");
	 *      LispStruct var6 = new IntIntegerStruct(var5);
	 *
	 *      if(var6 instanceof ValuesStruct) {
	 *          ValuesStruct var7 = (ValuesStruct)var6;
	 *          var6 = var7.getPrimaryValue();
	 *      }
	 *      var4.bindLexicalValue((LispStruct)var6);
	 *      var2.put(var4, var6);
	 *
	 *      LispStruct var10;
	 *      try {
	 *          PackageStruct var8 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var9 = var8.findSymbol("X").getSymbol();
	 *          var10 = var9.getLexicalValue();
	 *      } finally {
	 *          var4.unbindLexicalValue();
	 *      }
	 *      return var10;
	 * }
	 * }
	 * </pre>
	 *
	 * @param vars
	 * 		the {@link LetStruct.LetVar}s used to generate the {@link SymbolStruct} binding code
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 * @param closureSymbolBindingsStore
	 * 		the storage location index on the stack where the {@link Closure#symbolBindings} {@link Map} exists
	 * @param lexicalSymbolStoresToUnbind
	 * 		the {@link Set} of lexical {@link SymbolStruct} binding locations to unbind after the 'let' body executes
	 * @param dynamicSymbolStoresToUnbind
	 * 		the {@link Set} of dynamic {@link SymbolStruct} binding locations to unbind after the 'let' body executes
	 */
	@Override
	protected void generateBindings(final List<LetStruct.LetVar> vars, final GeneratorState generatorState,
	                                final JavaMethodBuilder methodBuilder, final int closureArgStore,
	                                final int closureSymbolBindingsStore, final Set<Integer> lexicalSymbolStoresToUnbind,
	                                final Set<Integer> dynamicSymbolStoresToUnbind) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();

		final Map<Integer, Integer> lexicalSymbolStoresToBind = new LinkedHashMap<>();
		final Map<Integer, Integer> dynamicSymbolStoresToBind = new LinkedHashMap<>();

		for (final LetStruct.LetVar var : vars) {
			final SymbolStruct symbolVar = var.getVar();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(symbolVar, generatorState, packageStore, symbolStore);

			final LispStruct initForm = var.getInitForm();
			initForm.generate(generatorState);

			final int initFormStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.VALUES_STRUCT_NAME,
			                   GenerationConstants.VALUES_STRUCT_EXTRACT_PRIMARY_VALUE_METHOD_NAME,
			                   GenerationConstants.VALUES_STRUCT_EXTRACT_PRIMARY_VALUE_METHOD_DESC,
			                   false);
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			final boolean isSpecial = var.isSpecial();
			if (isSpecial) {
				dynamicSymbolStoresToBind.put(symbolStore, initFormStore);
			} else {
				lexicalSymbolStoresToBind.put(symbolStore, initFormStore);
			}
		}

		// Add the symbolStores for the SymbolStructs to unbind
		lexicalSymbolStoresToUnbind.addAll(lexicalSymbolStoresToBind.keySet());
		dynamicSymbolStoresToUnbind.addAll(dynamicSymbolStoresToBind.keySet());

		for (final Map.Entry<Integer, Integer> functionStoreToBind : dynamicSymbolStoresToBind.entrySet()) {
			final Integer symbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_BIND_DYNAMIC_VALUE_METHOD_DESC,
			                   true);
		}

		for (final Map.Entry<Integer, Integer> functionStoreToBind : lexicalSymbolStoresToBind.entrySet()) {
			final Integer symbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.SYMBOL_STRUCT_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_NAME,
			                   GenerationConstants.SYMBOL_STRUCT_BIND_LEXICAL_VALUE_METHOD_DESC,
			                   true);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
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
	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(LET (");

		final List<LetStruct.LetVar> vars = getVars();
		final String varsPrinted =
				vars.stream()
				    .map(var -> '(' + var.getVar().toString() + ' ' + var.getInitForm().toString() + ')')
				    .collect(Collectors.joining(" "));
		builder.append(varsPrinted);
		builder.append(") ");

		final PrognStruct forms = getForms();
		final List<LispStruct> formsList = forms.getForms();
		for (final LispStruct form : formsList) {
			final String formPrint = form.toString();
			builder.append(formPrint);
		}

		builder.append(')');

		return builder.toString();
	}

	public static class LetVar {

		private final SymbolStruct var;

		private final LispStruct initForm;

		private final boolean isSpecial;

		public LetVar(final SymbolStruct var, final LispStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct getVar() {
			return var;
		}

		public LispStruct getInitForm() {
			return initForm;
		}

		public boolean isSpecial() {
			return isSpecial;
		}
	}
}
