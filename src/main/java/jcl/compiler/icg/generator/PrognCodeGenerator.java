/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.symbols.NILStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'progn' special operator code generation.
 */
@Component
class PrognCodeGenerator implements CodeGenerator<PrognStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating each of the {@link PrognStruct#forms} values.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@link NILCodeGenerator} used for generating a {@link NILStruct} when the {@link PrognStruct#forms} {@link
	 * List} is empty.
	 */
	@Autowired
	private NILCodeGenerator nilCodeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link PrognStruct} objects. If the {@link PrognStruct#forms} {@link List} is empty, a
	 * {@link NILStruct} is generated. Otherwise, each form value is generated via the {@link
	 * IntermediateCodeGenerator}. When iterating through and generating each of the forms, each of the form generation
	 * results will be popped off the top of the Stack, except for the final generated form.
	 *
	 * @param input
	 * 		the {@link PrognStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final PrognStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final List<LispStruct> forms = input.getForms();
		if (forms.isEmpty()) {
			nilCodeGenerator.generate(NILStruct.INSTANCE, generatorState);
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
