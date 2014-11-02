package jcl.compiler.real.icg;

import jcl.structs.characters.CharacterStruct;

public class CharacterCodeGenerator {

	public static void genCharacterStructCode(final IntermediateCodeGenerator icg, final CharacterStruct characterStruct) {
		icg.emitter.emitIconst(characterStruct.getCodePoint());
		icg.emitter.emitInvokestatic("jcl/structs/characters/CharacterStruct", "<init>", "(I)", "V", false);
	}
}
