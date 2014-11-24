package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.Keyword;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link KeywordSymbolStruct} is the object representation of a Lisp 'keyword' type.
 */
// TODO: should this be public?? Can we make it package visible??
public class KeywordSymbolStruct extends SymbolStruct<KeywordSymbolStruct> {

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

	@Override
	public String toString() {
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).setExcludeFieldNames("value").toString();
	}
}
