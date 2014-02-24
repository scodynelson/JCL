package jcl.structs.packages;

import jcl.structs.LispStruct;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.LispType;
import jcl.types.packages.Package;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PackageStruct implements LispStruct {

	private static final Map<String, PackageStruct> ALL_PACKAGES = new HashMap<>();

	public static final PackageStruct COMMON_LISP = getStruct("COMMON-LISP");
	public static final PackageStruct SYSTEM = getStruct("SYSTEM");
	public static final PackageStruct COMMON_LISP_USER = getStruct("COMMON-LISP-USER");
	public static final PackageStruct KEYWORD = getStruct("KEYWORD");

	static {
		ALL_PACKAGES.put("COMMON-LISP", COMMON_LISP);
		ALL_PACKAGES.put("SYSTEM", SYSTEM);
		ALL_PACKAGES.put("COMMON-LISP-USER", COMMON_LISP_USER);

		ALL_PACKAGES.put("KEYWORD", KEYWORD);
	}

	private final String name;
	private final List<String> nicknames;

	private final List<PackageStruct> useList = new ArrayList<>();
	private final List<PackageStruct> usedByList = new ArrayList<>();

	private final Map<String, SymbolStruct<?>> internalSymbols = new HashMap<>();
	private final Map<String, SymbolStruct<?>> inheritedSymbols = new HashMap<>();
	private final Map<String, SymbolStruct<?>> externalSymbols = new HashMap<>();
	private final Map<String, SymbolStruct<?>> shadowingSymbols = new HashMap<>();

	private PackageStruct(final String name, final List<String> nicknames) {
		this.name = name;
		this.nicknames = nicknames;
	}

	@Override
	public LispType getType() {
		return Package.INSTANCE;
	}

	public String getName() {
		return name;
	}

	public List<String> getNicknames() {
		return nicknames;
	}

	public SymbolStruct<?> intern(final String symbolName) {
		final SymbolStruct<?> symbolStruct = new SymbolStruct(symbolName);
		internalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return symbolStruct;
	}

	public SymbolStruct<?> findSymbol(final String symbolName) {

		// NOTE: Order matters here!!

		SymbolStruct<?> foundSymbol = externalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return foundSymbol;
		}

		foundSymbol = shadowingSymbols.get(symbolName);
		if (foundSymbol != null) {
			return foundSymbol;
		}

		foundSymbol = internalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return foundSymbol;
		}

		for (final PackageStruct currentPackage : useList) {
			final SymbolStruct<?> usedSymbol = currentPackage.externalSymbols.get(symbolName);

			if (usedSymbol != null) {
				return currentPackage.internalSymbols.get(symbolName);
			}
		}

		// it's not anywhere
		return null;
	}

	public SymbolStruct<?> findExternalSymbol(final String symbolName) {
		return externalSymbols.get(symbolName);
	}

	public static PackageStruct findPackage(final String packageName) {
		return ALL_PACKAGES.get(packageName);
	}

	@Override
	public String toString() {
		return "PackageStruct{" +
				"name='" + name + '\'' +
				", nicknames=" + nicknames +
				", useList=" + useList +
				", usedByList=" + usedByList +
				", internalSymbols=" + internalSymbols +
				", inheritedSymbols=" + inheritedSymbols +
				", externalSymbols=" + externalSymbols +
				", shadowingSymbols=" + shadowingSymbols +
				'}';
	}

	// BUILDERS

	public static PackageStruct getStruct(final String name) {
		final PackageStruct packageStruct = new PackageStruct(name, new ArrayList<String>());
		ALL_PACKAGES.put(name, packageStruct);
		return packageStruct;
	}

	public static PackageStruct getStruct(final String name, final List<String> nicknames) {
		final PackageStruct packageStruct = new PackageStruct(name, nicknames);
		ALL_PACKAGES.put(name, packageStruct);
		return packageStruct;
	}
}
