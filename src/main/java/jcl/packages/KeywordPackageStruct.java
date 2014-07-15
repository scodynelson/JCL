package jcl.packages;

import jcl.symbols.KeywordSymbolStruct;

/**
 * The {@code KeywordPackageStruct} is the object representation of a Lisp 'package' type specific for 'keyword' symbols.
 */
// TODO: should this be public?? Can we make it package visible??
public final class KeywordPackageStruct extends PackageStruct {

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
