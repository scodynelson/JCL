/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.LetStarStruct;
import jcl.functions.Closure;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'let*' special operator code generation. 'Let*' is different from 'Let' in that the binding of the
 * {@link SymbolStruct}s to their scoped {@link LispStruct} values occurs after the generate of its symbol and form
 * values instead of after all of the symbols and forms are generated. This allows sequentially defined variables to
 * depend on the binding of previous defined variables within the same variable scope definitions of the 'let*'.
 */
@Component
final class LetStarCodeGenerator extends ClosureCreationCodeGenerator<LetStarStruct.LetStarVar, LetStarStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link LetStarStruct.LetStarVar#initForm} values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Private constructor which passes 'letStar' as the prefix value to be set in it's {@link #methodNamePrefix}
	 * value.
	 */
	private LetStarCodeGenerator() {
		super("letStar");
	}

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<LetStarStruct> event) {
		super.onGeneratorEvent(event);
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link LetStarStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating each of the {@link LetStarStruct.LetStarVar#var} and {@link LetStarStruct.LetStarVar#initForm}
	 * values</li>
	 * <li>Collect all generated symbol and form stack locations for lazily binding the values to the {@link
	 * SymbolStruct}s</li>
	 * <li>Symbol bindings where {@link LetStarStruct.LetStarVar#isSpecial} is true are binded via {@link
	 * SymbolStruct#bindDynamicValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link LetStarStruct.LetStarVar#isSpecial} is false are binded via {@link
	 * SymbolStruct#bindLexicalValue(LispStruct)}</li>
	 * <li>Symbol bindings where {@link LetStarStruct.LetStarVar#isSpecial} is false are also added to the {@link
	 * Closure#symbolBindings} map for the new {@link Closure} created with the 'let'</li>
	 * </ol>
	 * As an example, it will transform {@code (let* ((x 1)) x)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct letStar_1(Closure var1) {
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
	 * 		the {@link LetStarStruct.LetStarVar}s used to generate the {@link SymbolStruct} binding code
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
	protected void generateBindings(final List<LetStarStruct.LetStarVar> vars, final GeneratorState generatorState,
	                                final JavaMethodBuilder methodBuilder, final int closureArgStore,
	                                final int closureSymbolBindingsStore, final Set<Integer> lexicalSymbolStoresToUnbind,
	                                final Set<Integer> dynamicSymbolStoresToUnbind) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();

		for (final LetStarStruct.LetStarVar var : vars) {
			final SymbolStruct symbolVar = var.getVar();
			final int symbolStore = methodBuilder.getNextAvailableStore();
			CodeGenerators.generateSymbol(symbolVar, generatorState, packageStore, symbolStore);

			final LispStruct initForm = var.getInitForm();
			codeGenerator.generate(initForm, generatorState);

			final int initFormStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.VALUES_STRUCTS_NAME,
			                   GenerationConstants.VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_NAME,
			                   GenerationConstants.VALUES_STRUCTS_EXTRACT_PRIMARY_VALUE_METHOD_DESC,
			                   false);
			mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

			mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
			mv.visitVarInsn(Opcodes.ALOAD, initFormStore);

			final boolean isSpecial = var.isSpecial();
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
	}
}
