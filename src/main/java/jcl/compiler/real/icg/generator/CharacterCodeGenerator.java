package jcl.compiler.real.icg.generator;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
class CharacterCodeGenerator implements CodeGenerator<CharacterStruct> {

	private static final String CHARACTER_STRUCT_NAME = Type.getInternalName(CharacterStruct.class);

	private static final String CHARACTER_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(CharacterStruct.class, int.class);

	@Override
	public void generate(final CharacterStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, CHARACTER_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		final int codePoint = input.getCodePoint();
		mv.visitLdcInsn(codePoint);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				CHARACTER_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				CHARACTER_STRUCT_INIT_DESC,
				false);
	}
}
