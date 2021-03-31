/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.icg.generator.ReturnFromException;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class ReturnFromStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct name;
	private final LispStruct result;

	public ReturnFromStruct(final SymbolStruct name, final LispStruct result) {
		super("returnFrom");
		this.name = name;
		this.result = result;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(RETURN-FROM ");

		final String namePrinted = name.toString();
		builder.append(namePrinted);

		builder.append(' ');

		final String formsPrinted = result.toString();
		builder.append(formsPrinted);

		builder.append(')');

		return builder.toString();
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ReturnFromStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Fetching the global 'COMMON-LISP-USER' package</li>
	 * <li>Finding the {@link SymbolStruct} with the {@link ReturnFromStruct#name} value in the fetched
	 * 'COMMON-LISP-USER' package</li>
	 * <li>Generating the {@link ReturnFromStruct#result} value</li>
	 * <li>Creating and throwing a new {@link ReturnFromException} with the {@link SymbolStruct} name and {@link
	 * LispStruct} result values</li>
	 * </ol>
	 * As an example, it will transform {@code (return-from foo 1)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct returnFrom_1(Environment var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      BigInteger var4 = new BigInteger("1");
	 *      IntegerStruct var5 = new IntegerStruct(var4);
	 *      throw new ReturnFromException(var3, var5);
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int environmentArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		// Generate the Name
		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(name, generatorState, namePackageStore, nameSymbolStore);

		// Generate the Result
		result.generate(generatorState);

		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		// Create and throw the ReturnFromException
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.RETURN_FROM_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.RETURN_FROM_EXCEPTION_INIT_DESC,
		                   false);
		mv.visitInsn(Opcodes.ATHROW);
	}
}
