/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.InternalEval;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import lombok.Getter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@Getter
public class PrognStruct extends CompilerSpecialOperatorStruct {

	private final List<LispStruct> forms;

	public PrognStruct(final List<LispStruct> forms) {
		super("progn");
		this.forms = forms;
	}

	@Override
	public String toString() {
		final String formsString = forms.stream()
		                                .map(Object::toString)
		                                .collect(Collectors.joining(" "));
		return "(PROGN " + formsString + ')';
	}

	@Override
	public LispStruct eval(final Environment environment) {
		LispStruct finalForm = NILStruct.INSTANCE;

		for (final LispStruct form : forms) {
			finalForm = InternalEval.eval(form);
		}
		return finalForm;
	}

	/**
	 * {@inheritDoc}
	 * Generation method for {@link PrognStruct} objects. If the {@link PrognStruct#forms} {@link List} is empty, a
	 * {@link NILStruct} is generated. Otherwise, each form value is generated. When iterating through and generating
	 * each of the forms, each of the form generation results will be popped off the top of the Stack, except for the
	 * final generated form.
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (forms.isEmpty()) {
			NILStruct.INSTANCE.generate(generatorState);
		} else {
			for (final Iterator<LispStruct> iterator = forms.iterator(); iterator.hasNext(); ) {

				final LispStruct form = iterator.next();
				form.generate(generatorState);
				if (iterator.hasNext()) {
					mv.visitInsn(Opcodes.POP);
				}
			}
		}
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {
		// Do Nothing.
	}
}
