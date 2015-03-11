package jcl.compiler.real.icg;

import jcl.lists.NullStruct;
import org.springframework.stereotype.Component;

@Component
public class NILCodeGenerator implements CodeGenerator<NullStruct> {

	@Override
	public void generate(final NullStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitGetstatic("jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
	}
}
