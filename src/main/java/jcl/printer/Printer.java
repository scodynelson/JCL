package jcl.printer;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;

public class Printer {

	public static String print(final LispStruct lispStruct) {
		if (lispStruct instanceof ListStruct) {
			return ListStructPrinter.print((ListStruct) lispStruct);
		} else if (lispStruct instanceof SymbolStruct) {
			return SymbolStructPrinter.print((SymbolStruct<?>) lispStruct);
		} else if (lispStruct instanceof IntegerStruct) {
			return IntegerStructPrinter.print((IntegerStruct) lispStruct);
		}

		return lispStruct.toString();
	}
}
