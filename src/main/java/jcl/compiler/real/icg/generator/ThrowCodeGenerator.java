package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class ThrowCodeGenerator extends SpecialOperatorCodeGenerator<ThrowStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	private static final String THROW_EXCEPTION_INIT_DESC = CodeGenerators.getConstructorDescription(ThrowException.class, LispStruct.class, LispStruct.class);

	private ThrowCodeGenerator() {
		super("throw");
	}

	@Override
	protected void generateSpecialOperator(final ThrowStruct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct catchTag = input.getCatchTag();
		final LispStruct resultForm = input.getResultForm();

		codeGenerator.generate(catchTag, generatorState);
		final int catchTagStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, catchTagStore);

		codeGenerator.generate(resultForm, generatorState);
		final int resultFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultFormStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.THROW_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, catchTagStore);
		mv.visitVarInsn(Opcodes.ALOAD, resultFormStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.THROW_EXCEPTION_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				THROW_EXCEPTION_INIT_DESC,
				false);
		mv.visitInsn(Opcodes.ATHROW);
	}
}
