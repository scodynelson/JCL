package jcl.printer;

import jcl.symbols.SymbolStruct;

public class SymbolStructPrinter {

	public static String print(final SymbolStruct<?> symbolStruct) {
		return symbolStruct.getName();
	}
}
