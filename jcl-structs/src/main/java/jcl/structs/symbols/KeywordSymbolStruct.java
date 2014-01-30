package jcl.structs.symbols;

import jcl.structs.conses.ListStruct;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.packages.PackageStruct;

import java.util.List;

public class KeywordSymbolStruct extends SymbolStruct<KeywordSymbolStruct> {

	public KeywordSymbolStruct(final String name) {
		super(name, PackageStruct.KEYWORD, null, null, null, true, true);
	}

	public KeywordSymbolStruct(final String name, final List<ListStruct> propertyList, final FunctionStruct function) {
		super(name, PackageStruct.KEYWORD, propertyList, null, function, true, true);
	}

	@Override
	public KeywordSymbolStruct getValue() {
		return this;
	}

	// BUILDERS

//	public static KeywordSymbolStruct getStruct(final String name) {
//		return new KeywordSymbolStruct(name, null, null);
//	}
//
//	public static KeywordSymbolStruct getStruct(final String name, final FunctionStruct function) {
//		return new KeywordSymbolStruct(name, null, function);
//	}
}
