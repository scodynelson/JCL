package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.Keyword;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link KeywordSymbolStruct} is the object representation of a Lisp 'keyword' type.
 */
// TODO: should this be public?? Can we make it package visible??
public class KeywordSymbolStruct extends SymbolStruct<KeywordSymbolStruct> {

	private static final long serialVersionUID = -8081437644901785951L;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	public KeywordSymbolStruct(final String name) {
		super(Keyword.INSTANCE, name, GlobalPackageStruct.KEYWORD, null, null);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		setValue(this);
	}

	// NOTE: These 3 standard methods below exclude the 'value' property to avoid circularities since the value of a
	//       KeywordSymbolStruct is itself.

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(name)
		                            .append(symbolPackage)
		                            .append(function)
		                            .append(properties)
		                            .append(macroFunctionExpander)
		                            .append(compilerMacroFunctionExpander)
		                            .append(symbolMacroExpander)
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
		final KeywordSymbolStruct rhs = (KeywordSymbolStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(name, rhs.name)
		                          .append(symbolPackage, rhs.symbolPackage)
		                          .append(function, rhs.function)
		                          .append(properties, rhs.properties)
		                          .append(macroFunctionExpander, rhs.macroFunctionExpander)
		                          .append(compilerMacroFunctionExpander, rhs.compilerMacroFunctionExpander)
		                          .append(symbolMacroExpander, rhs.symbolMacroExpander)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(name)
		                                                                .append(symbolPackage)
		                                                                .append(function)
		                                                                .append(properties)
		                                                                .append(macroFunctionExpander)
		                                                                .append(compilerMacroFunctionExpander)
		                                                                .append(symbolMacroExpander)
		                                                                .toString();
	}
}
