package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SymbolFunctionCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	//	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		// there are multiple ways to handle this
		// we add an optimization for calling a CL function
		// it becomes a static field reference instead of a runtime symbol lookup
		// +0 ->
		final Label label = new Label();
		mv.visitLabel(label);
		specialVariableCodeGenerator.generate(input, classBuilder);
		// invoke symbol.getFunction()
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/common/type/Symbol", "getFunction", "()Llisp/common/type/Function;", true);
		// if the symbol has defined less than 12 params, we can say that it takes that number of args
	}
}
