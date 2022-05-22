/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.Set;

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
 * Class to perform 'let*' special operator code generation. 'Let*' is different from 'Let' in that the binding of the
 * {@link SymbolStruct}s to their scoped {@link LispStruct} values occurs after the generate of its symbol and form
 * values instead of after all of the symbols and forms are generated. This allows sequentially defined variables to
 * depend on the binding of previous defined variables within the same variable scope definitions of the 'let*'.
 */
public class LetStarStruct extends BindingEnvironmentStruct {

	public LetStarStruct(final List<BindingVar> vars, final PrognStruct forms, final Environment letStarEnvironment) {
		super("letStar", vars, forms, letStarEnvironment);
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LetStarStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating each of the {@link BindingVar#var} and {@link BindingVar#initForm}
	 * values</li>
	 * <li>Collect all generated symbol and form stack locations for lazily binding the values to the {@link
	 * SymbolStruct}s</li>
	 * <li>Symbol bindings where {@link BindingVar#isSpecial} is true are binded via {@link
	 * SymbolStruct#bindDynamicValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link BindingVar#isSpecial} is false are binded via {@link
	 * SymbolStruct#bindLexicalValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link BindingVar#isSpecial} is false are also added to the {@link
	 * Environment#lexicalSymbolBindings} map for the new {@link Environment} created with the 'let'</li>
	 * </ol>
	 * As an example, it will transform {@code (let* ((x 1)) x)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct letStar_1(Environment var1) {
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
	 *          var9 = var1.getSymbolValue(var8);
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
	 * 		{@link JavaEnvironmentMethodBuilder} used for building a Java method body
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

		final int environmentStore = methodBuilder.getEnvironmentStore();
		final int packageStore = methodBuilder.getNextAvailableStore();

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

			mv.visitVarInsn(Opcodes.ALOAD, environmentStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

			final boolean isSpecial = var.isSpecial();
			if (isSpecial) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				                   GenerationConstants.ENVIRONMENT_NAME,
				                   GenerationConstants.ENVIRONMENT_BIND_DYNAMIC_VALUE_METHOD_NAME,
				                   GenerationConstants.ENVIRONMENT_BIND_DYNAMIC_VALUE_METHOD_DESC,
				                   false);

				dynamicSymbolStoresToUnbind.add(symbolStore);
			} else {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				                   GenerationConstants.ENVIRONMENT_NAME,
				                   GenerationConstants.ENVIRONMENT_BIND_LEXICAL_VALUE_METHOD_NAME,
				                   GenerationConstants.ENVIRONMENT_BIND_LEXICAL_VALUE_METHOD_DESC,
				                   false);

				lexicalSymbolStoresToUnbind.add(symbolStore);
			}
		}
	}
}
