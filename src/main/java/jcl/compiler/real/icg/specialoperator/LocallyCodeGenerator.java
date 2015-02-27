package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class LocallyCodeGenerator implements CodeGenerator<ConsElement> {

	public static final LocallyCodeGenerator INSTANCE = new LocallyCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		//TODO unimplemented 'locally'
	}
}
