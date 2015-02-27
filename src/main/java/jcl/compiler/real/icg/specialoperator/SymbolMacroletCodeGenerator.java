package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class SymbolMacroletCodeGenerator implements CodeGenerator<ConsElement> {

	public static final SymbolMacroletCodeGenerator INSTANCE = new SymbolMacroletCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		//TODO unimplemented 'symbol-macrolet'
	}
}
