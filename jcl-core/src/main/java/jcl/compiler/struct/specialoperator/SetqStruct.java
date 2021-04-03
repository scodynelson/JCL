/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.Map;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.ValuesStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class SetqStruct extends CompilerSpecialOperatorStruct {

	private final List<SetqPair> setqPairs;

	public SetqStruct(final List<SetqPair> setqPairs) {
		super("setq");
		this.setqPairs = setqPairs;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(SETQ");

		for (final SetqStruct.SetqPair setqPair : setqPairs) {
			builder.append(' ');

			final SymbolStruct var = setqPair.getVar();
			final String varPrinted = var.toString();
			builder.append(varPrinted);

			builder.append(' ');

			final LispStruct form = setqPair.getForm();
			final String formPrinted = form.toString();
			builder.append(formPrinted);
		}

		builder.append(')');

		return builder.toString();
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link SetqStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Retrieving the {@link List} of symbol bindings from the {@link Environment} parameter if the parameter is not
	 * null</li>
	 * <li>Generating each of the {@link SetqStruct#setqPairs}, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link SetqStruct.SetqPair#var} value</li>
	 * <li>Generating the {@link SetqStruct.SetqPair#form} value</li>
	 * <li>Retrieving the primary value via {@link ValuesStruct#getPrimaryValue()} if the generated form is a {@link
	 * ValuesStruct}</li>
	 * <li>Generating the code to set the {@link SymbolStruct} value (lexical, dynamic, or regular) based on the
	 * current {@link Environment} at the top of the {@link GeneratorState#environmentDeque}</li>
	 * <li>Inserting the generated {@link SetqStruct.SetqPair#var} as the key and the {@link SetqStruct.SetqPair#form}
	 * as the value as an entry in the {@link Environment#lexicalSymbolBindings} {@link Map}</li>
	 * </ol>
	 * </li>
	 * <li>Generating the code to load the last generated {@link SetqStruct.SetqPair#form} value to return</li>
	 * </ol>
	 * As an example, it will transform {@code (setq x 1)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct setq_1(Environment var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("X").getSymbol();
	 *      BigInteger var5 = new BigInteger("1");
	 *      LispStruct var4 = new IntegerStruct(var5);
	 *      if(var4 instanceof ValuesStruct) {
	 *          ValuesStruct var6 = (ValuesStruct)var4;
	 *          var4 = var6.getPrimaryValue();
	 *      }
	 *
	 *      var1.setSymbolValue(var3, var4);
	 *      return var4;
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 *        {@link JavaEnvironmentMethodBuilder} used for building a Java method body
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();
		final int environmentStore = methodBuilder.getEnvironmentStore();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		final int initFormStore = methodBuilder.getNextAvailableStore();

		for (final SetqStruct.SetqPair setqPair : setqPairs) {
			final SymbolStruct var = setqPair.getVar();
			CodeGenerators.generateSymbol(var, generatorState, packageStore, symbolStore);

			final LispStruct form = setqPair.getForm();
			form.generate(generatorState);
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

			if (generatorState.getLexicalSymbols().contains(var)) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				                   GenerationConstants.ENVIRONMENT_NAME,
				                   GenerationConstants.ENVIRONMENT_SET_LEXICAL_SYMBOL_VALUE_METHOD_NAME,
				                   GenerationConstants.ENVIRONMENT_SET_LEXICAL_SYMBOL_VALUE_METHOD_DESC,
				                   false);
			} else if (generatorState.getDynamicSymbols().contains(var)) {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				                   GenerationConstants.ENVIRONMENT_NAME,
				                   GenerationConstants.ENVIRONMENT_SET_DYNAMIC_SYMBOL_VALUE_METHOD_NAME,
				                   GenerationConstants.ENVIRONMENT_SET_DYNAMIC_SYMBOL_VALUE_METHOD_DESC,
				                   false);
			} else {
				mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				                   GenerationConstants.ENVIRONMENT_NAME,
				                   GenerationConstants.ENVIRONMENT_SET_SYMBOL_VALUE_METHOD_NAME,
				                   GenerationConstants.ENVIRONMENT_SET_SYMBOL_VALUE_METHOD_DESC,
				                   false);
			}
		}

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
		mv.visitInsn(Opcodes.ARETURN);
	}

	@Getter
	@AllArgsConstructor
	public static class SetqPair {
		private final SymbolStruct var;
		private final LispStruct form;
	}
}
