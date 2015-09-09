package jcl.compiler.real.icg.generator;

import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.lists.NullStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class PrognCodeGenerator implements CodeGenerator<PrognStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

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
				codeGenerator.generate(form, generatorState);
				if (iterator.hasNext()) {
					mv.visitInsn(Opcodes.POP);
				}
			}
		}
	}
}
