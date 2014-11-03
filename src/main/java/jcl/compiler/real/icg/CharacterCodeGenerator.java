package jcl.compiler.real.icg;

import jcl.structs.characters.CharacterStruct;

public class CharacterCodeGenerator implements CodeGenerator<CharacterStruct> {

	public static final CharacterCodeGenerator INSTANCE = new CharacterCodeGenerator();

	@Override
	public void generate(final CharacterStruct input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitIconst(input.getCodePoint());
		codeGenerator.emitter.emitInvokestatic("jcl/structs/characters/CharacterStruct", "<init>", "(I)", "V", false);
	}
}
