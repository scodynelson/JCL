package jcl.compiler.real.icg;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public class CharacterCodeGenerator implements CodeGenerator<CharacterStruct> {

	@Override
	public void generate(final CharacterStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitIconst(input.getCodePoint());
		classBuilder.getEmitter().emitInvokestatic("jcl/characters/CharacterStruct", "<init>", "(I)", "V", false);
	}
}
