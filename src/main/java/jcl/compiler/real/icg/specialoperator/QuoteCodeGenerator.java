package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.ComplexCodeGenerator;
import jcl.compiler.real.icg.FloatCodeGenerator;
import jcl.compiler.real.icg.IntegerCodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.RatioCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.numbers.ComplexStruct;
import jcl.structs.numbers.FloatStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.numbers.RatioStruct;
import jcl.structs.symbols.SymbolStruct;

public class QuoteCodeGenerator implements CodeGenerator<ListStruct> {

	// this method can ONLY handle simple constants such as numbers, strings,
	// and literal symbols

	public static final QuoteCodeGenerator INSTANCE = new QuoteCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		final Object quotedObj = input.getRest().getFirst();
		if (quotedObj instanceof SymbolStruct) {
			final SymbolStruct<?> sym = (SymbolStruct) quotedObj;
			//TODO work out a way to handle uninterned symbols that have been encountered already
			// need symbol package lookup here!
			if (sym.getSymbolPackage() == null) {
				codeGenerator.emitter.emitLdc(sym.getName());
				codeGenerator.emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
			} else {
				codeGenerator.genCodeSpecialVariable(sym);
			}
		} else if (quotedObj instanceof IntegerStruct) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerStruct) quotedObj, codeGenerator);
		} else if (quotedObj instanceof FloatStruct) {
			FloatCodeGenerator.INSTANCE.generate((FloatStruct) quotedObj, codeGenerator);
		} else if (quotedObj instanceof RatioStruct) {
			RatioCodeGenerator.INSTANCE.generate((RatioStruct) quotedObj, codeGenerator);
		} else if (quotedObj instanceof ComplexStruct) {
			ComplexCodeGenerator.INSTANCE.generate((ComplexStruct) quotedObj, codeGenerator);
		} else {
			throw new RuntimeException("Unable to quote: " + quotedObj);
		}
	}
}
