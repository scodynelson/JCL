package jcl.functions;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;

public final class FunctionHelpers {

	private FunctionHelpers() {
	}

	public static PackageStruct asPackage(final LispStruct lispStruct) {
		if (lispStruct instanceof PackageStruct) {
			return (PackageStruct) lispStruct;
		} else if (lispStruct instanceof SymbolStruct) {
			final SymbolStruct symbolStruct = (SymbolStruct) lispStruct;
			final String name = symbolStruct.getName();
			return PackageStruct.findPackage(name);
		} else if (lispStruct instanceof CharacterStruct) {
			final CharacterStruct characterStruct = (CharacterStruct) lispStruct;
			final String packageName = characterStruct.toJavaCharacter().toString();
			return PackageStruct.findPackage(packageName);
		} else if (lispStruct instanceof StringStruct) {
			final StringStruct stringStruct = (StringStruct) lispStruct;
			final String packageName = stringStruct.toJavaString();
			return PackageStruct.findPackage(packageName);
		} else {
			throw new TypeErrorException("Type cannot be converted to Package.");
		}
	}

	public static StringStruct asString(final LispStruct lispStruct) {
		if (lispStruct instanceof StringStruct) {
			return (StringStruct) lispStruct;
		} else if (lispStruct instanceof SymbolStruct) {
			final SymbolStruct symbolStruct = (SymbolStruct) lispStruct;
			final String name = symbolStruct.getName();
			return StringStruct.toLispString(name);
		} else if (lispStruct instanceof CharacterStruct) {
			final CharacterStruct characterStruct = (CharacterStruct) lispStruct;
			return StringStruct.toLispString(characterStruct.toJavaCharacter().toString());
		} else {
			throw new TypeErrorException("Type cannot be converted to Package.");
		}
	}
}
