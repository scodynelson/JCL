package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.BlockStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class BlockCodeGenerator extends SpecialOperatorCodeGenerator<BlockStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private BlockCodeGenerator() {
		super("block_");
	}

	@Override
	protected void generateSpecialOperator(final BlockStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final SymbolStruct<?> name = input.getName();
		final PrognStruct forms = input.getForms();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.RETURN_FROM_EXCEPTION_NAME);

		final int namePackageStore = methodBuilder.getNextAvailableStore();
		final int nameSymbolStore = methodBuilder.getNextAvailableStore();
		SymbolCodeGeneratorUtil.generate(name, generatorState, namePackageStore, nameSymbolStore);

		mv.visitLabel(tryBlockStart);
		prognCodeGenerator.generate(forms, generatorState);
		final int resultStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int returnFromExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, returnFromExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_NAME_METHOD_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_NAME_METHOD_DESC,
				false);
		final int returnFromExceptionNameStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, returnFromExceptionNameStore);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionNameStore);
		mv.visitVarInsn(Opcodes.ALOAD, nameSymbolStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.SYMBOL_STRUCT_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_NAME,
				GenerationConstants.JAVA_EQUALS_METHOD_DESC,
				false);

		final Label rethrowException = new Label();
		mv.visitJumpInsn(Opcodes.IFEQ, rethrowException);

		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.RETURN_FROM_EXCEPTION_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_NAME,
				GenerationConstants.RETURN_FROM_EXCEPTION_GET_RESULT_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(rethrowException);
		mv.visitVarInsn(Opcodes.ALOAD, returnFromExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
