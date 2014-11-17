package jcl.structs.packages;

import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Internal class for returning a {@link SymbolStruct} and it's current package symbol type as a {@link
 * KeywordSymbolStruct}.
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
	 * @param symbolStruct
	 * 		the symbol result
	 * @param packageSymbolType
	 * 		the symbol package location
	 */
	public PackageSymbolStruct(final SymbolStruct<?> symbolStruct, final KeywordSymbolStruct packageSymbolType) {
		this.symbolStruct = symbolStruct;
		this.packageSymbolType = packageSymbolType;
	}

	/**
	 * Getter for package-symbol {@link #symbolStruct} property.
	 *
	 * @return package-symbol {@link #symbolStruct} property
	 */
	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	/**
	 * Getter for package-symbol {@link #packageSymbolType} property.
	 *
	 * @return package-symbol {@link #packageSymbolType} property
	 */
	public KeywordSymbolStruct getPackageSymbolType() {
		return packageSymbolType;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
