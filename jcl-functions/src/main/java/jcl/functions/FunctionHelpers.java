package jcl.functions;

import java.io.File;

import com.ibm.icu.lang.UCharacter;
import jcl.lang.CharacterStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.CharacterStructImpl;
import jcl.lang.internal.PathnameStructImpl;
import jcl.lang.internal.StringStructImpl;

public final class FunctionHelpers {

	private FunctionHelpers() {
	}

	public static PathnameStruct asPathname(final LispStruct lispStruct) {
		if (lispStruct instanceof PathnameStruct) {
			return (PathnameStruct) lispStruct;
		} else if (lispStruct instanceof FileStreamStruct) {
			final FileStreamStruct fileStreamStruct = (FileStreamStruct) lispStruct;
			final File file = fileStreamStruct.getPath().toFile();
			final String namestring = file.getAbsolutePath();
			return PathnameStructImpl.valueOf(namestring);
		} else if (lispStruct instanceof StringStruct) {
			final StringStruct stringStruct = (StringStruct) lispStruct;
			final String namestring = stringStruct.getAsJavaString();
			return PathnameStructImpl.valueOf(namestring);
		} else {
			throw new TypeErrorException("Type cannot be converted to Pathname.");
		}
	}

	public static CharacterStruct asCharacter(final LispStruct lispStruct) {
		if (lispStruct instanceof CharacterStruct) {
			return (CharacterStruct) lispStruct;
		} else if (lispStruct instanceof SymbolStruct) {
			final SymbolStruct symbol = (SymbolStruct) lispStruct;
			final String name = symbol.getName();
			if (name.length() != 1) {
				throw new SimpleErrorException("Symbol name is not of length one: " + name);
			}
			return CharacterStructImpl.valueOf(name.charAt(0));
		} else if (lispStruct instanceof StringStruct) {
			final StringStruct struct = (StringStruct) lispStruct;
			final String javaString = struct.getAsJavaString();
			if (javaString.length() != 1) {
				throw new SimpleErrorException("String is not of length one: " + javaString);
			}
			return CharacterStructImpl.valueOf(javaString.charAt(0));
		} else {
			throw new TypeErrorException("Type cannot be converted to Character.");
		}
	}

	public static CharacterStruct asNamedCharacter(final LispStruct lispStruct) {
		if (lispStruct instanceof CharacterStruct) {
			return (CharacterStruct) lispStruct;
		} else if (lispStruct instanceof SymbolStruct) {
			final SymbolStruct symbol = (SymbolStruct) lispStruct;
			final String name = symbol.getName();
			return CharacterStructImpl.valueOf(UCharacter.getCharFromName(name));
		} else if (lispStruct instanceof StringStruct) {
			final StringStruct string = (StringStruct) lispStruct;
			final String javaString = string.getAsJavaString();
			return CharacterStructImpl.valueOf(UCharacter.getCharFromName(javaString));
		} else {
			throw new TypeErrorException("Type cannot be converted to Character.");
		}
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
			final String packageName = characterStruct.getCharacter().toString();
			return PackageStruct.findPackage(packageName);
		} else if (lispStruct instanceof StringStruct) {
			final StringStruct stringStruct = (StringStruct) lispStruct;
			final String packageName = stringStruct.getAsJavaString();
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
			return StringStructImpl.valueOf(name);
		} else if (lispStruct instanceof CharacterStruct) {
			final CharacterStruct characterStruct = (CharacterStruct) lispStruct;
			return StringStructImpl.valueOf(characterStruct.getCharacter().toString());
		} else {
			throw new TypeErrorException("Type cannot be converted to Package.");
		}
	}
}
