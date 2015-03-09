package jcl.packages;

import jcl.symbols.KeywordSymbolStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link KeywordPackageStruct} is the object representation of a Lisp 'package' type specific for 'keyword'
 * symbols.
 */
final class KeywordPackageStruct extends PackageStruct {

	/**
	 * Singleton instance of the Keyword package.
	 */
	public static final PackageStruct INSTANCE = new KeywordPackageStruct();

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5527737254864167846L;

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
		return new PackageSymbolStruct(symbolStruct, CommonLispSymbols.INTERNAL);
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
