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
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
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
public class LetStruct extends BindingEnvironmentStruct {

	public LetStruct(final List<BindingVar> vars, final PrognStruct forms, final Environment letEnvironment) {
		super("let", vars, forms, letEnvironment);
	}

	/**
	 * {@inheritDoc} Generation method for {@code LetStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating each of the {@link BindingVar#var} and {@link BindingVar#initForm} values</li>
	 * <li>Collect all generated symbol and form stack locations for lazily binding the values to the {@link
	 * SymbolStruct}s</li>
	 * <li>Symbol bindings where {@link BindingVar#isSpecial} is true are binded via {@link
	 * SymbolStruct#bindDynamicValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link BindingVar#isSpecial} is false are binded via {@link
	 * SymbolStruct#bindLexicalValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link BindingVar#isSpecial} is false are also added to the {@link
	 * Environment#lexicalSymbolBindings} map for the new {@link Environment} created with the 'let'</li>
	 * </ol>
	 * As an example, it will transform {@code (let ((x 1)) x)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct let_1(Environment var1) {
	 *      var1 = new Environment(var1);
	 *
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("X").getSymbol();
	 *
	 *      BigInteger var4 = new BigInteger("1");
	 *      LispStruct var5 = new IntegerStruct(var4);
	 *
	 *      if(var5 instanceof ValuesStruct) {
	 *          ValuesStruct var6 = (ValuesStruct)var5;
	 *          var5 = var6.getPrimaryValue();
	 *      }
	 *      var1.bindLexicalValue(var3, (LispStruct)var5);
	 *
	 *      LispStruct var9;
	 *      try {
	 *          PackageStruct var7 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *          SymbolStruct var8 = var7.findSymbol("X").getSymbol();
	 *          var9 = var1.geSymbolValue(var8);
	 *      } finally {
	 *          var1.unbindLexicalValue(var3);
	 *      }
	 *      return var9;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 *        {@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 * @param lexicalSymbolStoresToUnbind
	 * 		the {@link Set} of lexical {@link SymbolStruct} binding locations to unbind after the 'let' body executes
	 * @param dynamicSymbolStoresToUnbind
	 * 		the {@link Set} of dynamic {@link SymbolStruct} binding locations to unbind after the 'let' body executes
	 */
	@Override
	protected void generateBindings(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder,
	                                final Set<Integer> lexicalSymbolStoresToUnbind,
	                                final Set<Integer> dynamicSymbolStoresToUnbind) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();

		final Map<Integer, Integer> lexicalSymbolStoresToBind = new LinkedHashMap<>();
		final Map<Integer, Integer> dynamicSymbolStoresToBind = new LinkedHashMap<>();

		for (final BindingVar var : vars) {
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

		final int environmentStore = methodBuilder.getEnvironmentStore();

		// Add the symbolStores for the SymbolStructs to unbind
		lexicalSymbolStoresToUnbind.addAll(lexicalSymbolStoresToBind.keySet());
		dynamicSymbolStoresToUnbind.addAll(dynamicSymbolStoresToBind.keySet());

		for (final Map.Entry<Integer, Integer> functionStoreToBind : dynamicSymbolStoresToBind.entrySet()) {
			final Integer symbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_BIND_DYNAMIC_VALUE_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_BIND_DYNAMIC_VALUE_METHOD_DESC,
			                   false);
		}

		for (final Map.Entry<Integer, Integer> functionStoreToBind : lexicalSymbolStoresToBind.entrySet()) {
			final Integer symbolStore = functionStoreToBind.getKey();
			final Integer initFormStore = functionStoreToBind.getValue();

			mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.ENVIRONMENT_NAME,
			                   GenerationConstants.ENVIRONMENT_BIND_LEXICAL_VALUE_METHOD_NAME,
			                   GenerationConstants.ENVIRONMENT_BIND_LEXICAL_VALUE_METHOD_DESC,
			                   false);
		}
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(LET (");

		final List<BindingVar> vars = getVars();
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
}
