package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.NullStruct;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class NILCodeGenerator implements CodeGenerator<NullStruct> {

	@Override
	public void generate(final NullStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		currentClass.getMethodVisitor().visitFieldInsn(Opcodes.GETSTATIC, "jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
	}
}
