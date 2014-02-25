package jcl.structs.packages;

import jcl.structs.symbols.SymbolStruct;

public class KeywordPackageStruct extends PackageStruct {

	public static final PackageStruct INSTANCE = new KeywordPackageStruct();

	private KeywordPackageStruct() {
		super("KEYWORD");
	}

	@Override
	public PackageSymbolStruct intern(final String symbolName) {
		final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final SymbolStruct<?> symbolStruct = new SymbolStruct(symbolName);
		externalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, PackageSymbolType.INTERNAL);
	}
}
