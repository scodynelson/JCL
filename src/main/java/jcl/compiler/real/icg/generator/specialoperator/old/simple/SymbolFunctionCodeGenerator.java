package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class SymbolFunctionCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	//	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		specialVariableCodeGenerator.generate(input, classBuilder);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE, "lisp/common/type/Symbol", "getFunction", "()Llisp/common/type/Function;", true);
	}
}
