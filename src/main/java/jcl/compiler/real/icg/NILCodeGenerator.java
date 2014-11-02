package jcl.compiler.real.icg;

public class NILCodeGenerator {

	public static void genNIL(final IntermediateCodeGenerator icg) {
		icg.emitter.emitGetstatic("jcl/structs/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
	}
}
