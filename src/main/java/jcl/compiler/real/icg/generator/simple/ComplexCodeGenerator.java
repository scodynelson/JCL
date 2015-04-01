package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.numbers.ComplexStruct;
import jcl.numbers.RealStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ComplexCodeGenerator implements CodeGenerator<ComplexStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ComplexStruct input, final JavaClassBuilder classBuilder) {

		final RealStruct real = input.getReal();
		final RealStruct imaginary = input.getImaginary();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		formGenerator.generate(real, classBuilder);
		final int realStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, realStore);

		formGenerator.generate(imaginary, classBuilder);
		final int imaginaryStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, imaginaryStore);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/numbers/ComplexStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, realStore);
		mv.visitVarInsn(Opcodes.ALOAD, imaginaryStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/numbers/ComplexStruct", "<init>", "(Ljcl/numbers/RealStruct;Ljcl/numbers/RealStruct;)V", false);
	}
}
