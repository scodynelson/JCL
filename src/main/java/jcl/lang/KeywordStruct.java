package jcl.lang;

import jcl.lang.internal.KeywordStructImpl;
import jcl.lang.statics.GlobalPackageStruct;

/**
 * The {@link KeywordStruct} is the object representation of a Lisp 'keyword' type.
 */
public interface KeywordStruct extends SymbolStruct {

	/**
	 * Returns a new KeywordStruct with the provided {@link String} name.
	 *
	 * @param name
	 * 		the name of the new keyword
	 *
	 * @return a new KeywordStruct
	 */
	static KeywordStruct toLispKeyword(final String name) {
		final KeywordStructImpl struct = new KeywordStructImpl(name);

		struct.setfSymbolValue(struct);
		struct.setConstant();

		GlobalPackageStruct.KEYWORD.importSymbol(struct);
		GlobalPackageStruct.KEYWORD.export(struct);
		struct.setSymbolPackage(GlobalPackageStruct.KEYWORD);

		return struct;
	}
}
