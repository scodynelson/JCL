package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.simple.NullCodeGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.lists.NullStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PrognCodeGenerator implements CodeGenerator<PrognStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	@Override
	public void generate(final PrognStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		final List<LispStruct> forms = input.getForms();
		if (forms.isEmpty()) {
			nullCodeGenerator.generate(NullStruct.INSTANCE, classBuilder);
		} else {
			for (final Iterator<LispStruct> iterator = forms.iterator(); iterator.hasNext(); ) {

				final LispStruct form = iterator.next();
				if (form == null) {
					// Remove the current element from the iterator and the list.
					iterator.remove();
				}
				formGenerator.generate(form, classBuilder);
				if (iterator.hasNext()) {
					mv.visitInsn(Opcodes.POP);
				}
			}
		}
	}
}
