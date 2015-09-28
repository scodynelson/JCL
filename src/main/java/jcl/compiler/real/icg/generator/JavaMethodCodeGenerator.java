/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JCLClassWriter;
import jcl.compiler.real.icg.JCLMethodWriter;

public abstract class JavaMethodCodeGenerator<I extends LispStruct> implements CodeGenerator<I> {

	private final int access;

	private final String name;

	private final String desc;

	private final String signature;

	private final String[] exceptions;

	public JavaMethodCodeGenerator(final int access, final String name, final String desc) {
		this(access, name, desc, null, null);
	}

	public JavaMethodCodeGenerator(final int access, final String name, final String desc, final String signature,
	                               final String[] exceptions) {
		this.access = access;
		this.name = name;
		this.desc = desc;
		this.signature = signature;
		this.exceptions = exceptions;
	}

	@Override
	public void generate(final I input, final GeneratorState generatorState) {
		final JCLClassWriter cw = generatorState.getCurrentCw();
		final JCLMethodWriter mw = cw.visitMethod(access, name, desc, signature, exceptions);

		mw.visitCode();

		generateMethodContent(mw);

		mw.visitMaxs(-1, -1);
		mw.visitEnd();
	}

	protected abstract void generateMethodContent(JCLMethodWriter mw);
}
