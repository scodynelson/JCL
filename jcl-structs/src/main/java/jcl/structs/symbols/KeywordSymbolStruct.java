package jcl.structs.symbols;

import jcl.structs.functions.FunctionStruct;
import jcl.structs.packages.PackageStruct;
import jcl.types.symbols.Keyword;

import javax.annotation.PostConstruct;

public class KeywordSymbolStruct extends SymbolStruct<KeywordSymbolStruct> {

	public KeywordSymbolStruct(final String name) {
		this(name, null);
	}

	public KeywordSymbolStruct(final String name, final FunctionStruct function) {
		super(Keyword.INSTANCE, name, PackageStruct.KEYWORD, null, function);
	}

	@PostConstruct
	public void init() {
		setValue(this);
	}
}
