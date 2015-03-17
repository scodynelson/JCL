package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.Keyword;

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
}
