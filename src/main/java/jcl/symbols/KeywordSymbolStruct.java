package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.Keyword;

/**
 * The {@code KeywordSymbolStruct} is the object representation of a Lisp 'keyword' type.
 */
// TODO: should this be public?? Can we make it package visible??
public class KeywordSymbolStruct extends SymbolStruct<KeywordSymbolStruct> {

	/**
	 * Public constructor.
	 *
	 * @param name the symbol name
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
		return "KeywordSymbolStruct{"
				+ "name='" + name + '\''
				+ ", symbolPackage=" + symbolPackage
				+ ", properties=" + properties
//				+ ", value=" + value // NOTE: we are NOT enabling this because right now the printer for default toString() will be an infinite loop
				+ ", function=" + function
				+ '}';
	}
}