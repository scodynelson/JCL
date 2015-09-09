package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.numbers.ComplexStruct;
import jcl.numbers.RealStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class ComplexCodeGenerator implements CodeGenerator<ComplexStruct> {

	private static final String COMPLEX_STRUCT_NAME = Type.getInternalName(ComplexStruct.class);

	private static final String COMPLEX_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(ComplexStruct.class, RealStruct.class, RealStruct.class);

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Override
	public void generate(final ComplexStruct input, final GeneratorState generatorState) {

		final RealStruct real = input.getReal();
		final RealStruct imaginary = input.getImaginary();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		codeGenerator.generate(real, generatorState);
		final int realStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, realStore);

		codeGenerator.generate(imaginary, generatorState);
		final int imaginaryStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, imaginaryStore);

		mv.visitTypeInsn(Opcodes.NEW, COMPLEX_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, realStore);
		mv.visitVarInsn(Opcodes.ALOAD, imaginaryStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				COMPLEX_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				COMPLEX_STRUCT_INIT_DESC,
				false);
	}
}
