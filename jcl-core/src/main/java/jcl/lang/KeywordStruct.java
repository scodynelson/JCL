package jcl.lang;

import jcl.lang.internal.KeywordStructImpl;
import jcl.lang.statics.GlobalPackageStruct;

/**
 * The {@link KeywordStruct} is the object representation of a Lisp 'keyword' type.
 */
public interface KeywordStruct extends SymbolStruct {

	static KeywordStruct toLispKeyword(final String name) {
		final KeywordStructImpl struct = new KeywordStructImpl(name);

		struct.setSymbolValue(struct);
		struct.setConstant();

		GlobalPackageStruct.KEYWORD.importSymbol(struct);
		GlobalPackageStruct.KEYWORD.export(struct);
		struct.setSymbolPackage(GlobalPackageStruct.KEYWORD);

		return struct;
	}
}
