package jcl.compiler.real.icg;

import jcl.characters.CharacterStruct;

public class CharacterCodeGenerator implements CodeGenerator<CharacterStruct> {

	public static final CharacterCodeGenerator INSTANCE = new CharacterCodeGenerator();

	@Override
	public void generate(final CharacterStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitIconst(input.getCodePoint());
		classBuilder.getEmitter().emitInvokestatic("jcl/characters/CharacterStruct", "<init>", "(I)", "V", false);
	}
}
