package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LocallyEnvironment;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.LocallyStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LocallyCodeGenerator implements CodeGenerator<LocallyStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final LocallyStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		final LocallyEnvironment locallyEnvironment = input.getLocallyEnvironment();
		bindingStack.push(locallyEnvironment);

		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, classBuilder);

		bindingStack.pop();

		final int resultStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, resultStore);

		mv.visitVarInsn(Opcodes.ALOAD, resultStore);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
//		mv.visitInsn(Opcodes.ARETURN);
	}
}
