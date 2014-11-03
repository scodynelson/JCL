package jcl.compiler.real.icg;

import jcl.structs.symbols.NILStruct;

public class NILCodeGenerator implements CodeGenerator<NILStruct> {

	public static final NILCodeGenerator INSTANCE = new NILCodeGenerator();

	@Override
	public void generate(final NILStruct input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitGetstatic("jcl/structs/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
	}
}
