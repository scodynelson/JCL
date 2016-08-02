package jcl.lang;

import jcl.lang.statics.GlobalPackageStruct;
import jcl.type.KeywordType;

/**
 * The {@link KeywordStruct} is the object representation of a Lisp 'keyword' type.
 */
public final class KeywordStruct extends ConstantStruct<KeywordStruct> {

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	private KeywordStruct(final String name) {
		super(KeywordType.INSTANCE, name, GlobalPackageStruct.KEYWORD, null, null);
		init();
	}

	public static KeywordStruct valueOf(final String name) {
		return new KeywordStruct(name);
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		dynamicValueStack.push(this);
	}
}
