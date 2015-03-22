/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages;

import java.io.Serializable;

import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Internal class for returning a {@link SymbolStruct} and it's current package symbol type as a {@link
 * KeywordSymbolStruct}.
 */
public class PackageSymbolStruct implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8169693960575105621L;

	/**
	 * The {@link SymbolStruct} symbol.
	 */
	private final SymbolStruct<?> symbol;

	/**
	 * The {@link #symbol}s package location type.
	 */
	private final KeywordSymbolStruct packageSymbolType;

	/**
	 * Protected constructor.
	 *
	 * @param symbol
	 * 		the symbol result
	 * @param packageSymbolType
	 * 		the symbol package location
	 */
	public PackageSymbolStruct(final SymbolStruct<?> symbol, final KeywordSymbolStruct packageSymbolType) {
		this.symbol = symbol;
		this.packageSymbolType = packageSymbolType;
	}

	/**
	 * Getter for package-symbol {@link #symbol} property.
	 *
	 * @return package-symbol {@link #symbol} property
	 */
	public SymbolStruct<?> getSymbol() {
		return symbol;
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
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(symbol)
		                            .append(packageSymbolType)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final PackageSymbolStruct rhs = (PackageSymbolStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(symbol, rhs.symbol)
		                          .append(packageSymbolType, rhs.packageSymbolType)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(symbol)
		                                                                .append(packageSymbolType)
		                                                                .toString();
	}
}
