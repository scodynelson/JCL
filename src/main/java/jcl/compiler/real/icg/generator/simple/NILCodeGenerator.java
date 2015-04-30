package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.symbols.NILStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class NILCodeGenerator implements CodeGenerator<NILStruct> {

	private static final String NIL_STRUCT_NAME = Type.getInternalName(NILStruct.class);

	private static final String NIL_STRUCT_DESC = Type.getDescriptor(NILStruct.class);

	@Override
	public void generate(final NILStruct input, final JavaClassBuilder classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitFieldInsn(Opcodes.GETSTATIC, NIL_STRUCT_NAME, GenerationConstants.SINGLETON_INSTANCE, NIL_STRUCT_DESC);
	}
}
