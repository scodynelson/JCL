package jcl.compiler.real.icg.specialoperator;

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

public class QuoteCodeGenerator {

	// this method can ONLY handle simple constants such as numbers, strings,
	// and literal symbols
	public static void genCodeQuote(final IntermediateCodeGenerator icg, final ListStruct list) {
		final Object quotedObj = list.getRest().getFirst();
		if (quotedObj instanceof SymbolStruct) {
			final SymbolStruct<?> sym = (SymbolStruct) quotedObj;
			//TODO work out a way to handle uninterned symbols that have been encountered already
			// need symbol package lookup here!
			if (sym.getSymbolPackage() == null) {
				icg.emitter.emitLdc(sym.getName());
				icg.emitter.emitInvokestatic("lisp/common/type/Symbol$Factory", "newInstance", "(Ljava/lang/String;)", "Llisp/common/type/Symbol;", false);
			} else {
				icg.genCodeSpecialVariable(sym);
			}
		} else if (quotedObj instanceof IntegerStruct) {
			IntegerCodeGenerator.genCodeInteger(icg, (IntegerStruct) quotedObj);
		} else if (quotedObj instanceof FloatStruct) {
			FloatCodeGenerator.genCodeFloat(icg, (FloatStruct) quotedObj);
		} else if (quotedObj instanceof RatioStruct) {
			RatioCodeGenerator.genCodeRatio(icg, (RatioStruct) quotedObj);
		} else if (quotedObj instanceof ComplexStruct) {
			ComplexCodeGenerator.genCodeComplex(icg, (ComplexStruct) quotedObj);
		} else {
			throw new RuntimeException("Unable to quote: " + quotedObj);
		}
	}
}
