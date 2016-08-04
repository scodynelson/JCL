package jcl.lang.internal;

import jcl.lang.KeywordStruct;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.type.KeywordType;

/**
 * The {@link KeywordStructImpl} is the object representation of a Lisp 'keyword' type.
 */
public final class KeywordStructImpl extends ConstantStructImpl<KeywordStructImpl> implements KeywordStruct {

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	private KeywordStructImpl(final String name) {
		super(KeywordType.INSTANCE, name, GlobalPackageStruct.KEYWORD, null, null);
		init();
	}

	public static KeywordStruct valueOf(final String name) {
		return new KeywordStructImpl(name);
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		dynamicValueStack.push(this);
	}
}
