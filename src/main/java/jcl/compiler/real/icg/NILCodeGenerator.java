package jcl.compiler.real.icg;

import jcl.lists.NullStruct;

public class NILCodeGenerator implements CodeGenerator<NullStruct> {

	public static final NILCodeGenerator INSTANCE = new NILCodeGenerator();

	@Override
	public void generate(final NullStruct input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitGetstatic("jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
	}
}
