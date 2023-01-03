/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.ConsStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class QuoteStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct object;

	public QuoteStruct(final LispStruct object) {
		super("quote");
		this.object = object;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("'");

		final String objectPrinted = object.toString();
		builder.append(objectPrinted);

		return builder.toString();
	}

	@Override
	public LispStruct eval(final Environment environment) {
		return object;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link QuoteStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link QuoteStruct#object} value</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		generateQuotedObject(object, generatorState);
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {
		// Do Nothing.
	}

	/**
	 * Generation method for quoted {@link LispStruct} objects. It performs the following operations:
	 * <ol>
	 * <li>Generating the value via {@link #generateQuotedSymbol(SymbolStruct, GeneratorState)} if the {@code
	 * quotedObject} is a {@link SymbolStruct}</li>
	 * <li>Generating the value via {@link #generateQuotedCons(ConsStruct, GeneratorState)} if the {@code quotedObject}
	 * is a {@link ConsStruct}</li>
	 * <li>Generating the value via {@link LispStruct#generate(GeneratorState)} if the
	 * {@code
	 * quotedObject} is neither a {@link SymbolStruct} nor a {@link ConsStruct}</li>
	 * </ol>
	 *
	 * @param quotedObject
	 * 		the 'quoted' {@link LispStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@SuppressWarnings("ChainOfInstanceofChecks")
	private void generateQuotedObject(final LispStruct quotedObject, final GeneratorState generatorState) {
		if (quotedObject instanceof SymbolStruct) {
			generateQuotedSymbol((SymbolStruct) quotedObject, generatorState);
		} else if (quotedObject instanceof ConsStruct) {
			generateQuotedCons((ConsStruct) quotedObject, generatorState);
		} else {
			quotedObject.generate(generatorState);
		}
	}

	/**
	 * Generation method for quoted {@link SymbolStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link SymbolStruct} value</li>
	 * <li>Ensuring the generated {@link SymbolStruct} value is loaded on the top of the stack</li>
	 * </ol>
	 * As an example, it will transform {@code 'x} into the following Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("X").getSymbol();
	 * }
	 * </pre>
	 *
	 * @param quotedSymbol
	 * 		the 'quoted' {@link SymbolStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	private static void generateQuotedSymbol(final SymbolStruct quotedSymbol, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(quotedSymbol, generatorState, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
	}

	/**
	 * Generation method for quoted {@link ConsStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Checking whether or not the {@link ConsStruct} is circular, throwing a {@link ProgramErrorException} if
	 * so</li>
	 * <li>Looping throw the {@link ConsStruct} in reverse order using a {@link ListIterator}, generating each element
	 * into its appropriate embedded {@link ConsStruct}</li>
	 * <li>Creating a dotted {@link ConsStruct} for the final 2 elements first if the last element is not NIL</li>
	 * </ol>
	 * As an example, it will transform {@code '(x)} into the following Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("X").getSymbol();
	 *      ConsStruct var5 = new ConsStruct(var3, NILStruct.INSTANCE);
	 * }
	 * </pre>
	 *
	 * @param quotedCons
	 * 		the 'quoted' {@link ConsStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	private void generateQuotedCons(final ConsStruct quotedCons, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (NILStruct.INSTANCE.eql(quotedCons.listLength())) {
			throw new ProgramErrorException("Generation of circular lists is not yet supported.");
		}

		final List<LispStruct> lispStructs = quotedCons.stream().collect(Collectors.toList());
		final ListIterator<LispStruct> listIterator = lispStructs.listIterator(lispStructs.size());

		LispStruct previousCdr = listIterator.previous();
		generateQuotedObject(previousCdr, generatorState);

		final int lastElementStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, lastElementStore);

		final LispStruct last = quotedCons.last(IntegerStruct.ZERO);
		if (NILStruct.INSTANCE.eq(last)) {
			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
			mv.visitFieldInsn(Opcodes.GETSTATIC,
			                  GenerationConstants.NIL_STRUCT_NAME,
			                  GenerationConstants.SINGLETON_INSTANCE,
			                  GenerationConstants.NIL_STRUCT_DESC);
		} else {
			previousCdr = listIterator.previous();
			generateQuotedObject(previousCdr, generatorState);

			final int secondToLastElementStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, secondToLastElementStore);

			mv.visitVarInsn(Opcodes.ALOAD, secondToLastElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
		}
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.CONS_STRUCT_NAME,
		                   GenerationConstants.CONS_STRUCT_TO_CONS_METHOD_NAME,
		                   GenerationConstants.CONS_STRUCT_TO_CONS_METHOD_DESC,
		                   true);

		final int previousConsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);

		final int nextElementStore = methodBuilder.getNextAvailableStore();

		while (listIterator.hasPrevious()) {
			previousCdr = listIterator.previous();
			generateQuotedObject(previousCdr, generatorState);

			mv.visitVarInsn(Opcodes.ASTORE, nextElementStore);

			mv.visitVarInsn(Opcodes.ALOAD, nextElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.CONS_STRUCT_NAME,
			                   GenerationConstants.CONS_STRUCT_TO_CONS_METHOD_NAME,
			                   GenerationConstants.CONS_STRUCT_TO_CONS_METHOD_DESC,
			                   true);
			mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);
		}

		mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
	}
}
