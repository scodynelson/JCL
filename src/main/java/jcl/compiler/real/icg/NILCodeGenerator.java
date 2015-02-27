package jcl.compiler.real.icg;

import jcl.compiler.real.element.NullElement;

public class NILCodeGenerator implements CodeGenerator<NullElement> {

	public static final NILCodeGenerator INSTANCE = new NILCodeGenerator();

	@Override
	public void generate(final NullElement input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitGetstatic("jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
	}
}
