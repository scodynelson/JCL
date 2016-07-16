package jcl.lang;

import jcl.type.KeywordType;

/**
 * The {@link KeywordStruct} is the object representation of a Lisp 'keyword' type.
 */
public class KeywordStruct extends ConstantStruct<KeywordStruct> {

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the symbol name
	 */
	public KeywordStruct(final String name) {
		super(KeywordType.INSTANCE, name, GlobalPackageStruct.KEYWORD, null, null);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		dynamicValueStack.push(this);
	}
}
