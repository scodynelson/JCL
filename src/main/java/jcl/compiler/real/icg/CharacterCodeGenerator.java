package jcl.compiler.real.icg;

import jcl.compiler.real.element.CharacterElement;

public class CharacterCodeGenerator implements CodeGenerator<CharacterElement> {

	public static final CharacterCodeGenerator INSTANCE = new CharacterCodeGenerator();

	@Override
	public void generate(final CharacterElement input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitIconst(input.getCodePoint());
		codeGenerator.emitter.emitInvokestatic("jcl/characters/CharacterStruct", "<init>", "(I)", "V", false);
	}
}
