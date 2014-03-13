package jcl.reader.tokens;

import jcl.structs.SymbolStruct;

public interface SpecialOperatorSymbolTokens {

	SymbolStruct<?> QUOTE = new SymbolStruct("QUOTE");
	SymbolStruct<?> FUNCTION = new SymbolStruct("FUNCTION");

}
