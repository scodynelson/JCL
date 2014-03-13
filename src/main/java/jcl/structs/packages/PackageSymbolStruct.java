package jcl.structs.packages;

import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SymbolStruct;

/**
 * Internal class for returning a SymbolStruct and it's current package symbol type as a KeywordSymbolStruct.
 */
public class PackageSymbolStruct {

	public static final KeywordSymbolStruct INTERNAL = new KeywordSymbolStruct("INTERNAL");
	public static final KeywordSymbolStruct EXTERNAL = new KeywordSymbolStruct("EXTERNAL");
	public static final KeywordSymbolStruct INHERITED = new KeywordSymbolStruct("INHERITED");

	private final SymbolStruct<?> symbolStruct;
	private final KeywordSymbolStruct packageSymbolType;

	/**
	 * Protected constructor.
	 *
	 * @param symbolStruct      the symbol result
	 * @param packageSymbolType the symbol package location
	 */
	PackageSymbolStruct(final SymbolStruct<?> symbolStruct, final KeywordSymbolStruct packageSymbolType) {
		this.symbolStruct = symbolStruct;
		this.packageSymbolType = packageSymbolType;
	}

	/**
	 * Getter for package-symbol symbolStruct property.
	 *
	 * @return package-symbol symbolStruct property
	 */
	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	/**
	 * Getter for package-symbol packageSymbolType property.
	 *
	 * @return package-symbol packageSymbolType property
	 */
	public KeywordSymbolStruct getPackageSymbolType() {
		return packageSymbolType;
	}

	@Override
	public String toString() {
		return "PackageSymbolStruct{"
				+ "symbolStruct=" + symbolStruct
				+ ", packageSymbolType=" + packageSymbolType
				+ '}';
	}
}
