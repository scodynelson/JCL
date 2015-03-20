package jcl.compiler.real.icg.generator.simple;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class CharacterCodeGenerator implements CodeGenerator<CharacterStruct> {

	@Override
	public void generate(final CharacterStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "characterGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label getCharacterStruct = new Label();
		mv.visitLabel(getCharacterStruct);
//		mv.visitLineNumber(105, getCharacterStruct);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);

		final int codePoint = input.getCodePoint();
		mv.visitLdcInsn(codePoint);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 0);
		mv.visitEnd();
	}
}
