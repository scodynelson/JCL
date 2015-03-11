package jcl.compiler.real.icg.generator;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class CharacterCodeGenerator implements CodeGenerator<CharacterStruct> {

	@Override
	public void generate(final CharacterStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		final int codePoint = input.getCodePoint();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor methodVisitor = currentClass.getMethodVisitor();

		methodVisitor.visitLdcInsn(codePoint);
		methodVisitor.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
	}
}
