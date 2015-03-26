package jcl.compiler.real.icg.generator.specialoperator.old.compiler;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.FunctionCallCodeGenerator;
import jcl.lists.ListStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

//@Component
public class TailRecursionCodeGenerator implements CodeGenerator<ListStruct> {

	//	@Autowired
	private FunctionCallCodeGenerator functionCallCodeGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitVarInsn(Opcodes.ALOAD, 0);

		final boolean acceptsMultipleValues = classBuilder.isAcceptsMultipleValues();
		try {
			classBuilder.setAcceptsMultipleValues(false);
//			functionCallCodeGenerator.generate(input, classBuilder); TODO
		} finally {
			classBuilder.setAcceptsMultipleValues(acceptsMultipleValues);
		}
	}
}
