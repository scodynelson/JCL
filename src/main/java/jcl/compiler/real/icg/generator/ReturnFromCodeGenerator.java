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

@Component
class ReturnFromCodeGenerator extends SpecialOperatorCodeGenerator<ReturnFromStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	private static final String RETURN_FROM_EXCEPTION_INIT_DESC = CodeGenerators.getConstructorDescription(ReturnFromException.class, SymbolStruct.class, LispStruct.class);

	private ReturnFromCodeGenerator() {
		super("returnFrom");
	}

	@Override
	protected void generateSpecialOperator(final ReturnFromStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final SymbolStruct<?> name = input.getName();
		final LispStruct result = input.getResult();

		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		SymbolCodeGeneratorUtil.generate(name, generatorState, namePackageStore, nameSymbolStore);

		codeGenerator.generate(result, generatorState);
		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.RETURN_FROM_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				RETURN_FROM_EXCEPTION_INIT_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);
	}
}
