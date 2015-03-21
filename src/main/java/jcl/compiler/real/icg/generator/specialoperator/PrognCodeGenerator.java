package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PrognCodeGenerator implements CodeGenerator<PrognStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final PrognStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();

		final List<LispStruct> forms = input.getForms();
		for (final Iterator<LispStruct> iterator = forms.iterator(); iterator.hasNext(); ) {

			final LispStruct form = iterator.next();
			if (form == null) {
				// Remove the current element from the iterator and the list.
				iterator.remove();
			}
			formGenerator.generate(form, classBuilder);
			if (iterator.hasNext()) {
				currentClass.getMethodVisitor().visitInsn(Opcodes.POP);
			}
		}
	}
}
