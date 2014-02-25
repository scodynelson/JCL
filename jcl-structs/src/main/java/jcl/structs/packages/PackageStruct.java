package jcl.structs.packages;

import jcl.structs.LispStruct;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.LispType;
import jcl.types.packages.Package;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PackageStruct implements LispStruct {

	private static final Map<String, PackageStruct> ALL_PACKAGES = new HashMap<>();

	public static final PackageStruct COMMON_LISP = new PackageStruct("COMMON-LISP");
	public static final PackageStruct SYSTEM = new PackageStruct("SYSTEM");
	public static final PackageStruct COMMON_LISP_USER = new PackageStruct("COMMON-LISP-USER");
	public static final PackageStruct KEYWORD = new PackageStruct("KEYWORD");

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

	public PackageStruct(final String name) {
		this(name, new ArrayList<String>());
	}

	public PackageStruct(final String name, final List<String> nicknames) {
		this.name = name;
		this.nicknames = nicknames;
	}

	@PostConstruct
	public void init() {
		ALL_PACKAGES.put(name, this);
	}

	@PreDestroy
	public void destroy() {
		ALL_PACKAGES.remove(name);
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
}
