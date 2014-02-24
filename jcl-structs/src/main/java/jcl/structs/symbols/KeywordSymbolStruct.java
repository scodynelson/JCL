package jcl.structs.symbols;

import jcl.structs.conses.ListStruct;
import jcl.structs.functions.FunctionStruct;
import jcl.structs.packages.PackageStruct;
import jcl.types.symbols.Keyword;

import java.util.List;

public class KeywordSymbolStruct extends SymbolStruct<KeywordSymbolStruct> {

	public KeywordSymbolStruct(final String name) {
		this(name, null, null);
	}

	public KeywordSymbolStruct(final String name, final List<ListStruct> propertyList, final FunctionStruct function) {
		super(Keyword.INSTANCE, name, PackageStruct.KEYWORD, propertyList, null, function, true, true);
	}

	@Override
	public KeywordSymbolStruct getValue() {
		return this;
	}
}
