package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
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
	public void generate(final PrognStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final List<LispStruct> forms = input.getForms();
		if (forms.isEmpty()) {
			nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);
		} else {
			for (final Iterator<LispStruct> iterator = forms.iterator(); iterator.hasNext(); ) {

				final LispStruct form = iterator.next();
				formGenerator.generate(form, generatorState);
				if (iterator.hasNext()) {
					mv.visitInsn(Opcodes.POP);
				}
			}
		}
	}
}
