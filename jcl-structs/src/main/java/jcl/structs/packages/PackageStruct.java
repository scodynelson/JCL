package jcl.structs.packages;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.conditions.exceptions.PackageErrorException;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.packages.Package;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class PackageStruct extends BuiltInClassStruct {

	private static final Map<String, PackageStruct> ALL_PACKAGES = new HashMap<>();

	public static final PackageStruct COMMON_LISP = new PackageStruct("COMMON-LISP");
	public static final PackageStruct SYSTEM = new PackageStruct("SYSTEM");
	public static final PackageStruct COMMON_LISP_USER = new PackageStruct("COMMON-LISP-USER");
	public static final PackageStruct KEYWORD = KeywordPackageStruct.INSTANCE;

	static {
		ALL_PACKAGES.put("COMMON-LISP", COMMON_LISP);
		ALL_PACKAGES.put("SYSTEM", SYSTEM);
		ALL_PACKAGES.put("COMMON-LISP-USER", COMMON_LISP_USER);

		ALL_PACKAGES.put("KEYWORD", KEYWORD);
	}

	private String name;
	private List<String> nicknames;

	private final Set<PackageStruct> useList;
	private final Set<PackageStruct> usedByList = new HashSet<>();

	private final Map<String, SymbolStruct<?>> internalSymbols = new HashMap<>();
	private final Map<String, SymbolStruct<?>> inheritedSymbols = new HashMap<>();

	// NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols
	// (aka. anything in ExternalSymbols or ShadowingSymbols are in InternalSymbols but not vice-versa)
	protected final Map<String, SymbolStruct<?>> externalSymbols = new HashMap<>();
	private final Map<String, SymbolStruct<?>> shadowingSymbols = new HashMap<>();

	public PackageStruct(final String name) {
		this(name, new ArrayList<String>());
	}

	public PackageStruct(final String name, final List<String> nicknames) {
		super(Package.INSTANCE, null, null);
		this.name = name;
		this.nicknames = nicknames;
		useList = new HashSet<>();
	}

	public PackageStruct(final String name, final List<String> nicknames, final Set<PackageStruct> useList) throws PackageErrorException {
		super(Package.INSTANCE, null, null);
		this.name = name;
		this.nicknames = nicknames;

		this.useList = useList;
		final PackageStruct[] useListArray = new PackageStruct[useList.size()];
		internalUserPackage(useList.toArray(useListArray));
	}

	@PostConstruct
	public void init() {
		ALL_PACKAGES.put(name, this);
	}

	@PreDestroy
	public void destroy() {
		ALL_PACKAGES.remove(name);
	}

	public String getName() {
		return name;
	}

	public List<String> getNicknames() {
		return nicknames;
	}

	public Map<String, SymbolStruct<?>> getExternalSymbols() {
		return externalSymbols;
	}

	public Map<String, SymbolStruct<?>> getShadowingSymbols() {
		return shadowingSymbols;
	}

	public List<PackageStruct> getUseList() {
		return new ArrayList<>(useList);
	}

	public List<PackageStruct> getUsedByList() {
		return new ArrayList<>(usedByList);
	}

	public void renamePackage(final String newName) {
		ALL_PACKAGES.remove(name);
		name = newName;
		ALL_PACKAGES.put(newName, this);
	}

	public void renamePackage(final String newName, final List<String> newNicknames) {
		ALL_PACKAGES.remove(name);
		name = newName;
		nicknames = newNicknames;
		ALL_PACKAGES.put(newName, this);
	}

	public void deletePackage() {
		ALL_PACKAGES.remove(name);
		for (final PackageStruct usedByPackage : usedByList) {
			usedByPackage.unUsePackage(this);
		}
		usedByList.clear();
		useList.clear();
		name = null;
	}

	public void usePackage(final PackageStruct... packagesToUse) throws PackageErrorException {
		useList.addAll(Arrays.asList(packagesToUse));
		internalUserPackage(packagesToUse);
	}

	private void internalUserPackage(final PackageStruct... packagesToUse) throws PackageErrorException {
		for (final PackageStruct packageToUse : packagesToUse) {
			if (packageToUse.equals(KEYWORD)) {
				throw new PackageErrorException(this + " can't use package " + KEYWORD);
			}

			// Shadow current nonInherited symbols
			for (final String symbolName : packageToUse.externalSymbols.keySet()) {
				final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
				if (nonInheritedPackageSymbol != null) {
					final SymbolStruct<?> nonInheritedSymbol = nonInheritedPackageSymbol.getSymbolStruct();
					shadowingSymbols.put(symbolName, nonInheritedSymbol);
				}
			}

			inheritedSymbols.putAll(packageToUse.externalSymbols);
			packageToUse.usedByList.add(this);
		}
	}

	public void unUsePackage(final PackageStruct... packagesToUnUse) {
		useList.removeAll(Arrays.asList(packagesToUnUse));
		for (final PackageStruct packageToUnUse : packagesToUnUse) {
			// NOTE: We will leave the shadows in the shadowing list. This is due to the fact that we would have to search
			// through ALL used packages to make sure that there aren't any other inherited symbols that the symbol names
			// are shadowing. That's just overkill, when keeping them in the shadowing list won't affect anything.

			final Set<String> externalSymbolNames = packageToUnUse.externalSymbols.keySet();
			inheritedSymbols.keySet().removeAll(externalSymbolNames);
			packageToUnUse.usedByList.remove(this);
		}
	}

	public PackageSymbolStruct findSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		final PackageSymbolStruct foundPackageSymbol = findNonInheritedSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final SymbolStruct<?> foundSymbol = inheritedSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolType.INHERITED);
		}

		return null;
	}

	public void importSymbols(final SymbolStruct<?>... symbols) {
		for (final SymbolStruct<?> symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
			if (nonInheritedPackageSymbol == null) {
				continue;
			}

			internalSymbols.put(symbolName, symbol);
			if (inheritedSymbols.containsKey(symbolName)) {
				shadowingSymbols.put(symbolName, symbol);
			}

			if (symbol.getSymbolPackage() == null) {
				symbol.setSymbolPackage(this);
			}
		}
	}

	public void shadowingImport(final SymbolStruct<?>... symbols) throws PackageErrorException {
		for (final SymbolStruct<?> symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
			if (nonInheritedPackageSymbol != null) {
				final SymbolStruct<?> nonInheritedSymbol = nonInheritedPackageSymbol.getSymbolStruct();
				unintern(nonInheritedSymbol);
			}

			internalSymbols.put(symbolName, symbol);
			shadowingSymbols.put(symbolName, symbol);

			if (symbol.getSymbolPackage() == null) {
				symbol.setSymbolPackage(this);
			}
		}
	}

	public void export(final SymbolStruct<?>... symbols) throws PackageErrorException {
		final List<String> notFoundSymbolNames = new ArrayList<>();

		for (final SymbolStruct<?> symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
			if (foundPackageSymbol == null) {
				notFoundSymbolNames.add(symbolName);
				continue;
			}

			if (externalSymbols.containsKey(symbolName)) {
				continue; // go to next symbol. already external
			}

			inheritedSymbols.remove(symbolName);
			importSymbols(symbol); // This will put the symbol in the "InternalSymbols" and possibly "ShadowingSymbols"
			externalSymbols.put(symbolName, symbol);

			// NOTE: We CAN do this it seems, but we're not required to. Should we though???
//			for (final PackageStruct usedByPackage : usedByList) {
//				usedByPackage.inheritedSymbols.put(symbolName, symbol);
//			}
		}

		if (!notFoundSymbolNames.isEmpty()) {
			final StringBuilder exceptionStringBuilder
					= new StringBuilder("The following symbols are not accessible in package " + this + ": (");
			for (final String notFoundSymbolName : notFoundSymbolNames) {
				exceptionStringBuilder.append(notFoundSymbolName);
				exceptionStringBuilder.append(' ');
			}
			exceptionStringBuilder.append(')');
			throw new PackageErrorException(exceptionStringBuilder.toString());
		}
	}

	public void unexport(final SymbolStruct<?>... symbols) throws PackageErrorException {
		final List<String> notFoundSymbolNames = new ArrayList<>();

		for (final SymbolStruct<?> symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
			if (foundPackageSymbol == null) {
				notFoundSymbolNames.add(symbolName);
				continue;
			}

			externalSymbols.remove(symbolName);

			for (final PackageStruct usedPackaged : usedByList) {
				usedPackaged.inheritedSymbols.remove(symbolName);
			}
		}

		if (!notFoundSymbolNames.isEmpty()) {
			final StringBuilder exceptionStringBuilder
					= new StringBuilder("The following symbols are not accessible in package " + this + ": (");
			for (final String notFoundSymbolName : notFoundSymbolNames) {
				exceptionStringBuilder.append(notFoundSymbolName);
				exceptionStringBuilder.append(' ');
			}
			exceptionStringBuilder.append(')');
			throw new PackageErrorException(exceptionStringBuilder.toString());
		}
	}

	public void shadow(final String... symbolNames) {
		for (final String symbolName : symbolNames) {
			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);

			SymbolStruct<?> nonInheritedSymbol = nonInheritedPackageSymbol.getSymbolStruct();
			if (nonInheritedSymbol == null) {
				nonInheritedSymbol = new SymbolStruct(symbolName);
				internalSymbols.put(symbolName, nonInheritedSymbol);
				nonInheritedSymbol.setSymbolPackage(this);
			}
			shadowingSymbols.put(symbolName, nonInheritedSymbol);
		}
	}

	public PackageSymbolStruct intern(final String symbolName) {
		final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final SymbolStruct<?> symbolStruct = new SymbolStruct(symbolName);
		internalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, PackageSymbolType.INTERNAL);
	}

	public boolean unintern(final SymbolStruct<?> symbol) throws PackageErrorException {
		final String symbolName = symbol.getName();

		// Test for conflicts BEFORE we remove anything
		final Set<SymbolStruct<?>> shadowingConflicts = getShadowingConflicts(symbolName);
		if (shadowingConflicts != null) {
			final StringBuilder exceptionStringBuilder
					= new StringBuilder("Uninterning " + symbolName + " from " + this + " would cause conflicts among : (");
			for (final SymbolStruct<?> conflictingSymbol : shadowingConflicts) {
				exceptionStringBuilder.append(conflictingSymbol);
				exceptionStringBuilder.append(' ');
			}
			exceptionStringBuilder.append(')');
			throw new PackageErrorException(exceptionStringBuilder.toString());
		}

		final SymbolStruct<?> externalSymbol = externalSymbols.remove(symbolName);
		final SymbolStruct<?> shadowingSymbol = shadowingSymbols.remove(symbolName);
		final SymbolStruct<?> internalSymbol = internalSymbols.remove(symbolName);

		symbol.setSymbolPackage(null);

		return (externalSymbol != null) || (shadowingSymbol != null) || (internalSymbol != null);
	}

	private Set<SymbolStruct<?>> getShadowingConflicts(final String symbolName) {
		if (!shadowingSymbols.containsKey(symbolName)) {
			return null;
		}

		final Set<SymbolStruct<?>> conflictingInheritedSymbols = new HashSet<>();
		for (final PackageStruct usedPackage : useList) {
			final PackageSymbolStruct inheritedPackageSymbol = usedPackage.findSymbol(symbolName);
			if (inheritedPackageSymbol != null) {
				final SymbolStruct<?> inheritedSymbol = inheritedPackageSymbol.getSymbolStruct();
				conflictingInheritedSymbols.add(inheritedSymbol);
			}
		}
		return (conflictingInheritedSymbols.size() > 1) ? conflictingInheritedSymbols : null;
	}

	public static PackageStruct findPackage(final String packageName) {
		return ALL_PACKAGES.get(packageName);
	}

	public static List<SymbolStruct<?>> findAllSymbols(final String symbolName) {
		final Set<SymbolStruct<?>> allSymbols = new HashSet<>();
		for (final PackageStruct packageStruct : ALL_PACKAGES.values()) {
			final PackageSymbolStruct foundPackageSymbol = packageStruct.findSymbol(symbolName);
			final SymbolStruct<?> foundSymbol = foundPackageSymbol.getSymbolStruct();
			allSymbols.add(foundSymbol);
		}
		return new ArrayList<>(allSymbols);
	}

	public static List<PackageStruct> listAllPackages() {
		return new ArrayList<>(ALL_PACKAGES.values());
	}

	private PackageSymbolStruct findNonInheritedSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		SymbolStruct<?> foundSymbol = externalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolType.EXTERNAL);
		}

		foundSymbol = shadowingSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolType.INTERNAL);
		}

		foundSymbol = internalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolType.INTERNAL);
		}

		return null;
	}

	public enum PackageSymbolType {
		INTERNAL,
		EXTERNAL,
		INHERITED
	}

	public static class PackageSymbolStruct {

		private final SymbolStruct<?> symbolStruct;
		private final PackageSymbolType packageSymbolType; // TODO: eventually, this should be one of the 3 standard "keyword" symbols

		protected PackageSymbolStruct(final SymbolStruct<?> symbolStruct, final PackageSymbolType packageSymbolType) {
			this.symbolStruct = symbolStruct;
			this.packageSymbolType = packageSymbolType;
		}

		public SymbolStruct<?> getSymbolStruct() {
			return symbolStruct;
		}

		public PackageSymbolType getPackageSymbolType() {
			return packageSymbolType;
		}

		@Override
		public String toString() {
			return "PackageSymbolStruct{" +
					"symbolStruct=" + symbolStruct +
					", packageSymbolType=" + packageSymbolType +
					'}';
		}
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
