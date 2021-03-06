/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.Map;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.ValuesStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SetqStruct extends CompilerSpecialOperatorStruct {

	private final List<SetqPair> setqPairs;

	public SetqStruct(final List<SetqPair> setqPairs) {
		super("setq");
		this.setqPairs = setqPairs;
	}

	public List<SetqPair> getSetqPairs() {
		return setqPairs;
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
	 * <li>Retrieving the {@link List} of symbol bindings from the {@link Closure} parameter if the parameter is not
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
	 * as the value as an entry in the {@link Closure#symbolBindings} {@link Map} if the {@link Closure} parameter is
	 * not null</li>
	 * </ol>
	 * </li>
	 * <li>Generating the code to load the last generated {@link SetqStruct.SetqPair#form} value to return</li>
	 * </ol>
	 * As an example, it will transform {@code (setq x 1)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct setq_1(Closure var1) {
	 *      Map var2 = null;
	 *      if(var1 != null) {
	 *          var2 = var1.getSymbolBindings();
	 *      }
	 *
	 *      PackageStruct var3 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var4 = var3.findSymbol("X").getSymbol();
	 *      BigInteger var6 = new BigInteger("1");
	 *      LispStruct var5 = new IntIntegerStruct(var6);
	 *      if(var5 instanceof ValuesStruct) {
	 *          ValuesStruct var7 = (ValuesStruct)var5;
	 *          var5 = var7.getPrimaryValue();
	 *      }
	 *
	 *      var4.setValue(var5);
	 *      if(var2 != null) {
	 *          var2.put(var4, var5);
	 *      }
	 *      return var5;
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
		final int closureSymbolBindingsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingsStore);

		final Label closureNullCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		mv.visitJumpInsn(Opcodes.IFNULL, closureNullCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.CLOSURE_NAME,
		                   GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_NAME,
		                   GenerationConstants.CLOSURE_GET_SYMBOL_BINDINGS_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ASTORE, closureSymbolBindingsStore);

		mv.visitLabel(closureNullCheckIfEnd);

		final Environment currentEnvironment = generatorState.getCurrentEnvironment();

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

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

			final boolean hasLexicalBinding = currentEnvironment.hasLexicalBinding(var);
			final boolean hasDynamicBinding = currentEnvironment.hasDynamicBinding(var);

			if (hasLexicalBinding) {
				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				                   GenerationConstants.SYMBOL_STRUCT_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_SET_LEXICAL_VALUE_METHOD_DESC,
				                   true);
			} else if (hasDynamicBinding) {
				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				                   GenerationConstants.SYMBOL_STRUCT_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_SET_DYNAMIC_VALUE_METHOD_DESC,
				                   true);
			} else {
				mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
				                   GenerationConstants.SYMBOL_STRUCT_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_SET_VALUE_METHOD_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_SET_VALUE_METHOD_DESC,
				                   true);
			}

			final Label closureBindingsNullCheckIfEnd = new Label();

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
			mv.visitJumpInsn(Opcodes.IFNULL, closureBindingsNullCheckIfEnd);

			mv.visitVarInsn(Opcodes.ALOAD, closureSymbolBindingsStore);
			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.JAVA_MAP_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_NAME,
			                   GenerationConstants.JAVA_MAP_PUT_METHOD_DESC,
			                   true);
			mv.visitInsn(Opcodes.POP);

			mv.visitLabel(closureBindingsNullCheckIfEnd);
		}
		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}

	public static class SetqPair {

		private final SymbolStruct var;

		private final LispStruct form;

		public SetqPair(final SymbolStruct var, final LispStruct form) {
			this.var = var;
			this.form = form;
		}

		public SymbolStruct getVar() {
			return var;
		}

		public LispStruct getForm() {
			return form;
		}
	}
}
