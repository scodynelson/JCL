package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.FloatElement;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.RatioElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.FloatCodeGenerator;
import jcl.compiler.real.icg.IntegerCodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.RatioCodeGenerator;

public class QuoteCodeGenerator implements CodeGenerator<ConsElement> {

	// this method can ONLY handle simple constants such as numbers, strings,
	// and literal symbols

	public static final QuoteCodeGenerator INSTANCE = new QuoteCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		final Object quotedObj = input.getElements().getAllButFirst().getFirst();
		if (quotedObj instanceof SymbolElement) {
			final SymbolElement sym = (SymbolElement) quotedObj;
			//TODO work out a way to handle uninterned symbols that have been encountered already
			// need symbol package lookup here!
			if (sym.getPackageName() == null) {
				codeGenerator.emitter.emitLdc(sym.getSymbolName());
				codeGenerator.emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
			} else {
				codeGenerator.genCodeSpecialVariable(sym);
			}
		} else if (quotedObj instanceof IntegerElement) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerElement) quotedObj, codeGenerator);
		} else if (quotedObj instanceof FloatElement) {
			FloatCodeGenerator.INSTANCE.generate((FloatElement) quotedObj, codeGenerator);
		} else if (quotedObj instanceof RatioElement) {
			RatioCodeGenerator.INSTANCE.generate((RatioElement) quotedObj, codeGenerator);
//		} else if (quotedObj instanceof ComplexStruct) {
//			ComplexCodeGenerator.INSTANCE.generate((ComplexStruct) quotedObj, codeGenerator);
		} else {
			throw new RuntimeException("Unable to quote: " + quotedObj);
		}
	}
}
