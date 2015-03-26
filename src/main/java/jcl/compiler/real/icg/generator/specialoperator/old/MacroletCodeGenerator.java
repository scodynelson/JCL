package jcl.compiler.real.icg.generator.specialoperator.old;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

//@Component
public class MacroletCodeGenerator implements CodeGenerator<ListStruct> {

	//	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		// Get rid of the MACROLET symbol
		ListStruct restOfList = input.getRest();

        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			formGenerator.generate(restOfList.getFirst(), classBuilder);
			restOfList = restOfList.getRest();
			if (!restOfList.equals(NullStruct.INSTANCE)) {
				mv.visitInsn(Opcodes.POP);
			}
		}
	}
}
