package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.ReturnFromStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'return-from' special operator code generation.
 */
@Component
final class ReturnFromCodeGenerator extends SpecialOperatorCodeGenerator<ReturnFromStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link ReturnFromStruct#name} and {@link
	 * ReturnFromStruct#result} values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * Private constructor which passes 'returnFrom' as the prefix value to be set in it's {@link #methodNamePrefix}
	 * value.
	 */
	private ReturnFromCodeGenerator() {
		super("returnFrom");
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
	 * private LispStruct returnFrom_1(Closure var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("FOO").getSymbol();
	 *      BigInteger var4 = new BigInteger("1");
	 *      IntegerStruct var5 = new IntegerStruct(var4);
	 *      throw new ReturnFromException(var3, var5);
	 * }
	 * }
	 * </pre>
	 *
	 * @param input
	 * 		the {@link ReturnFromStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	protected void generateSpecialOperator(final ReturnFromStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		// Generate the Name
		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		final SymbolStruct<?> name = input.getName();
		SymbolCodeGeneratorUtil.generate(name, generatorState, namePackageStore, nameSymbolStore);

		// Generate the Result
		final LispStruct result = input.getResult();
		codeGenerator.generate(result, generatorState);

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
