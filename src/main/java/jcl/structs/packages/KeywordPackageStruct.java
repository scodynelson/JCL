package jcl.structs.packages;

import jcl.structs.symbols.KeywordSymbolStruct;

/**
 * The {@link KeywordPackageStruct} is the object representation of a Lisp 'package' type specific for 'keyword' symbols.
 */
final class KeywordPackageStruct extends PackageStruct {

	public static final PackageStruct INSTANCE = new KeywordPackageStruct();

	/**
	 * Public constructor.
	 */
	private KeywordPackageStruct() {
		super("KEYWORD");
	}

	@Override
	public PackageSymbolStruct intern(final String symbolName) {
		final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final KeywordSymbolStruct symbolStruct = new KeywordSymbolStruct(symbolName);
		externalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, PackageSymbolStruct.INTERNAL);
	}
}
