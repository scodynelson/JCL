package jcl.reader.tokens;

import jcl.structs.symbols.SymbolStruct;

public interface SpecialOperatorSymbolTokens {

	SymbolStruct<?> QUOTE = SymbolStruct.getStruct("QUOTE");
	SymbolStruct<?> FUNCTION = SymbolStruct.getStruct("FUNCTION");

}
