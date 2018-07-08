package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.java.JavaNameStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
public class JavaNameCodeGenerator implements CodeGenerator<JavaNameStruct> {

	@Override
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<JavaNameStruct> event) {
		final JavaNameStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String javaName = input.getJavaName();
		mv.visitLdcInsn(javaName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_JAVA_NAME_METHOD_NAME,
		                   GenerationConstants.LISP_STRUCT_FACTORY_TO_JAVA_NAME_METHOD_DESC,
		                   false);
	}
}
