package jcl.packages;

import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.PackageErrorException;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.Package;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@link PackageStruct} is the object representation of a Lisp 'package' type.
 */
public class PackageStruct extends BuiltInClassStruct {

	private static final long serialVersionUID = -1802514718401723443L;

	private String name;
	private List<String> nicknames;

	private final Set<PackageStruct> useList;
	private final Set<PackageStruct> usedByList = new HashSet<>();

	private final Map<String, SymbolStruct<?>> internalSymbols = new ConcurrentHashMap<>();

	// NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols
	// (aka. anything in ExternalSymbols or ShadowingSymbols are in InternalSymbols but not vice-versa)
	protected final Map<String, SymbolStruct<?>> externalSymbols = new ConcurrentHashMap<>();
	private final Map<String, SymbolStruct<?>> shadowingSymbols = new ConcurrentHashMap<>();

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 */
	public PackageStruct(final String name) {
		this(name, new ArrayList<>());
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 * @param nicknames
	 * 		the package nicknames
	 */
	public PackageStruct(final String name, final List<String> nicknames) {
		super(Package.INSTANCE, null, null);
		this.name = name;
		this.nicknames = nicknames;
		useList = new HashSet<>();
	}

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 * @param nicknames
	 * 		the package nicknames
	 * @param useList
	 * 		the packages this package will use/inherit from
	 */
	public PackageStruct(final String name, final List<String> nicknames, final Set<PackageStruct> useList) {
		super(Package.INSTANCE, null, null);
		this.name = name;
		this.nicknames = nicknames;

		this.useList = useList;
		final PackageStruct[] useListArray = new PackageStruct[useList.size()];
		internalUsePackage(useList.toArray(useListArray));

		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		GlobalPackageStruct.ALL_PACKAGES.put(name, this);
	}

	/**
	 * Getter for package {@link #name} property.
	 *
	 * @return package {@link #name} property
	 */
	public String getName() {
		return name;
	}

	/**
	 * Getter for package {@link #nicknames} property.
	 *
	 * @return package {@link #nicknames} property
	 */
	public List<String> getNicknames() {
		return nicknames;
	}

	/**
	 * Getter for package {@link #externalSymbols} property.
	 *
	 * @return package {@link #externalSymbols} property
	 */
	public Map<String, SymbolStruct<?>> getExternalSymbols() {
		return externalSymbols;
	}

	/**
	 * Getter for package {@link #shadowingSymbols} property.
	 *
	 * @return package {@link #shadowingSymbols} property
	 */
	public Map<String, SymbolStruct<?>> getShadowingSymbols() {
		return shadowingSymbols;
	}

	/**
	 * Getter for package {@link #useList} property.
	 *
	 * @return package {@link #useList} property
	 */
	public List<PackageStruct> getUseList() {
		return new ArrayList<>(useList);
	}

	/**
	 * Getter for package {@link #usedByList} property.
	 *
	 * @return package {@link #usedByList} property
	 */
	public List<PackageStruct> getUsedByList() {
		return new ArrayList<>(usedByList);
	}

	/**
	 * Renames the package and updates it in the {@link GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @param newName
	 * 		the new package name
	 */
	public void renamePackage(final String newName) {
		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		name = newName;
		GlobalPackageStruct.ALL_PACKAGES.put(newName, this);
	}

	/**
	 * Renames the package and updates it in the global ALL_PACKAGES map.
	 *
	 * @param newName
	 * 		the new package name
	 * @param newNicknames
	 * 		the new package nicknames
	 */
	public void renamePackage(final String newName, final List<String> newNicknames) {
		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		name = newName;
		nicknames = newNicknames;
		GlobalPackageStruct.ALL_PACKAGES.put(newName, this);
	}

	/**
	 * Deletes the package and removes it from the {@link GlobalPackageStruct#ALL_PACKAGES} map. The deletion method
	 * does not destroy the object, but clears its global usages and changes the name to 'null'.
	 */
	public void deletePackage() {
		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		for (final PackageStruct usedByPackage : usedByList) {
			usedByPackage.unUsePackage(this);
		}
		usedByList.clear();
		useList.clear();
		name = null;
	}

	/**
	 * Updates the package to use the provided {@code packagesToUse}.
	 *
	 * @param packagesToUse
	 * 		the packages that will be used
	 */
	public void usePackage(final PackageStruct... packagesToUse) {
		useList.addAll(Arrays.asList(packagesToUse));
		internalUsePackage(packagesToUse);
	}

	/**
	 * Internal implementation of 'use-package' for updating the package to use the provided {@code packagesToUse}.
	 *
	 * @param packagesToUse
	 * 		the packages that will be used
	 */
	private void internalUsePackage(final PackageStruct... packagesToUse) {
		for (final PackageStruct packageToUse : packagesToUse) {
			if (packageToUse.equals(GlobalPackageStruct.KEYWORD)) {
				throw new PackageErrorException(this + " can't use package " + GlobalPackageStruct.KEYWORD);
			}

			// Shadow current nonInherited symbols
			for (final String symbolName : packageToUse.externalSymbols.keySet()) {
				final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
				if (nonInheritedPackageSymbol != null) {
					final SymbolStruct<?> nonInheritedSymbol = nonInheritedPackageSymbol.getSymbolStruct();
					shadowingSymbols.put(symbolName, nonInheritedSymbol);
				}
			}

//			packageToUse.usedByList.add(this); // TODO: fix this. StackOverflow here. why?
		}
	}

	/**
	 * Updates the package to un-use the provided {@code packagesToUnUse}.
	 *
	 * @param packagesToUnUse
	 * 		the packages that will be un-used
	 */
	public void unUsePackage(final PackageStruct... packagesToUnUse) {
		useList.removeAll(Arrays.asList(packagesToUnUse));
		for (final PackageStruct packageToUnUse : packagesToUnUse) {
			// NOTE: We will leave the shadows in the shadowing list. This is due to the fact that we would have to search
			// through ALL used packages to make sure that there aren't any other inherited symbols that the symbol names
			// are shadowing. That's just overkill, when keeping them in the shadowing list won't affect anything.
			packageToUnUse.usedByList.remove(this);
		}
	}

	/**
	 * Locates the symbol matching the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the symbol if found and it's package location type, or null if not found
	 */
	public PackageSymbolStruct findSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		final PackageSymbolStruct foundPackageSymbol = findNonInheritedSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final SymbolStruct<?> foundSymbol = findInheritedSymbol(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolStruct.INHERITED);
		}

		return null;
	}

	/**
	 * Imports the provided {@code symbols} into the package.
	 *
	 * @param symbols
	 * 		the symbols to import into the package
	 */
	public void importSymbols(final SymbolStruct<?>... symbols) {
		for (final SymbolStruct<?> symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
			if (nonInheritedPackageSymbol != null) {
				continue;
			}

			internalSymbols.put(symbolName, symbol);

			final SymbolStruct<?> foundSymbol = findInheritedSymbol(symbolName);
			if (foundSymbol != null) {
				shadowingSymbols.put(symbolName, symbol);
			}

			if (symbol.getSymbolPackage() == null) {
				symbol.setSymbolPackage(this);
			}
		}
	}

	/**
	 * Performs a shadowing import of the provided {@code symbols} into the package, shadowing each one and uninterning
	 * current symbols with matching symbol names that already exist in the internal symbols of the package.
	 *
	 * @param symbols
	 * 		the symbols to shadow import into the package
	 */
	public void shadowingImport(final SymbolStruct<?>... symbols) {
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

	/**
	 * Exports the provided symbols and puts them into the externalSymbols map. All found symbols are exported and
	 * those
	 * not found are stored for throwing in a {@link PackageErrorException}.
	 *
	 * @param symbols
	 * 		the symbols to export
	 */
	public void export(final SymbolStruct<?>... symbols) {
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

	/**
	 * Un-exports the provided symbols and removes them from the externalSymbols map. All found symbols are un-exported
	 * and those not found are stored for throwing in a {@link PackageErrorException}.
	 *
	 * @param symbols
	 * 		the symbols to un-export
	 */
	public void unexport(final SymbolStruct<?>... symbols) {
		final List<String> notFoundSymbolNames = new ArrayList<>();

		for (final SymbolStruct<?> symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
			if (foundPackageSymbol == null) {
				notFoundSymbolNames.add(symbolName);
				continue;
			}

			externalSymbols.remove(symbolName);
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

	/**
	 * Shadows the provided {@code symbolNames}, either by finding the current non-inherited matching symbol, or by
	 * creating a new symbol with the non-existent symbolName.
	 *
	 * @param symbolNames
	 * 		the names of the symbols to shadow
	 */
	public void shadow(final String... symbolNames) {
		for (final String symbolName : symbolNames) {
			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);

			SymbolStruct<?> nonInheritedSymbol = nonInheritedPackageSymbol.getSymbolStruct();
			if (nonInheritedSymbol == null) {
				nonInheritedSymbol = new SymbolStruct<>(symbolName);
				internalSymbols.put(symbolName, nonInheritedSymbol);
				nonInheritedSymbol.setSymbolPackage(this);
			}
			shadowingSymbols.put(symbolName, nonInheritedSymbol);
		}
	}

	/**
	 * Locates the symbol matching the provided {@code symbolName} or creates a new internal symbol with it, interns it
	 * into the package, and returns it with it's package location type.
	 *
	 * @param symbolName
	 * 		the name of the symbol to intern
	 *
	 * @return the symbol if found and it's package location type, or a new symbol with internal type if not found
	 */
	public PackageSymbolStruct intern(final String symbolName) {
		final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final SymbolStruct<?> symbolStruct = new SymbolStruct<>(symbolName);
		internalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, PackageSymbolStruct.INTERNAL);
	}

	/**
	 * Un-interns the provided {@code symbol} from the package.
	 *
	 * @param symbol
	 * 		the symbol to un-intern from the package
	 *
	 * @return whether a symbol was indeed un-interned or not
	 */
	public boolean unintern(final SymbolStruct<?> symbol) {
		final String symbolName = symbol.getName();

		// Test for conflicts BEFORE we remove anything
		final Set<SymbolStruct<?>> shadowingConflicts = getShadowingConflicts(symbolName);
		if (shadowingConflicts.size() > 1) {
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

	/**
	 * Finds the matching package in the {@link GlobalPackageStruct#ALL_PACKAGES} map by the provided {@code
	 * packageName}.
	 *
	 * @param packageName
	 * 		the name of the package to location within the {@link GlobalPackageStruct#ALL_PACKAGES} map
	 *
	 * @return the located package for the provided {@code packageName}
	 */
	public static PackageStruct findPackage(final String packageName) {
		return GlobalPackageStruct.ALL_PACKAGES.get(packageName);
	}

	/**
	 * This static method finds all symbols in the {@link GlobalPackageStruct#ALL_PACKAGES} map by traversing all the
	 * existing packages to find all the symbols with the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol(s) to locate within the {@link GlobalPackageStruct#ALL_PACKAGES} map
	 *
	 * @return the located symbol(s) within the ALL_PACKAGES global package map
	 */
	public static List<SymbolStruct<?>> findAllSymbols(final String symbolName) {
		final Set<SymbolStruct<?>> allSymbols = new HashSet<>();
		for (final PackageStruct packageStruct : GlobalPackageStruct.ALL_PACKAGES.values()) {
			final PackageSymbolStruct foundPackageSymbol = packageStruct.findSymbol(symbolName);
			final SymbolStruct<?> foundSymbol = foundPackageSymbol.getSymbolStruct();
			allSymbols.add(foundSymbol);
		}
		return new ArrayList<>(allSymbols);
	}

	/**
	 * Lists all current packages existent in the {@link GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @return a list of all current packages existent in the {@link GlobalPackageStruct#ALL_PACKAGES} map
	 */
	public static List<PackageStruct> listAllPackages() {
		return new ArrayList<>(GlobalPackageStruct.ALL_PACKAGES.values());
	}

	/**
	 * Determines if a name conflict exists with the symbolName and that it is currently resolved due to a shadowing
	 * symbol existence.
	 *
	 * @param symbolName
	 * 		the name of the symbol to check for shadowing conflicts
	 *
	 * @return the conflicting symbols if any exist, or null if no conflicts exist
	 */
	private Set<SymbolStruct<?>> getShadowingConflicts(final String symbolName) {
		if (!shadowingSymbols.containsKey(symbolName)) {
			return Collections.emptySet();
		}

		final Set<SymbolStruct<?>> conflictingInheritedSymbols = new HashSet<>();
		for (final PackageStruct usedPackage : useList) {
			final PackageSymbolStruct inheritedPackageSymbol = usedPackage.findSymbol(symbolName);
			if (inheritedPackageSymbol != null) {
				final SymbolStruct<?> inheritedSymbol = inheritedPackageSymbol.getSymbolStruct();
				conflictingInheritedSymbols.add(inheritedSymbol);
			}
		}
		return conflictingInheritedSymbols;
	}

	/**
	 * Locates the non-inherited symbol matching the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the symbol if found and it's package location type, or null if not found
	 */
	private PackageSymbolStruct findNonInheritedSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		SymbolStruct<?> foundSymbol = externalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolStruct.EXTERNAL);
		}

		foundSymbol = shadowingSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolStruct.INTERNAL);
		}

		foundSymbol = internalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, PackageSymbolStruct.INTERNAL);
		}

		return null;
	}

	/**
	 * Locates the inherited symbol matching the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the symbol if found, or null if not found
	 */
	private SymbolStruct<?> findInheritedSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		SymbolStruct<?> foundSymbol = null;
		for (final PackageStruct usedPackage : useList) {
			final PackageSymbolStruct inheritedPackageSymbol = usedPackage.findSymbol(symbolName);
			if (inheritedPackageSymbol == null) {
				continue;
			}

			final KeywordSymbolStruct packageSymbolType = inheritedPackageSymbol.getPackageSymbolType();
			if (PackageSymbolStruct.EXTERNAL.equals(packageSymbolType)) {
				foundSymbol = inheritedPackageSymbol.getSymbolStruct();
				break;
			}
		}

		return foundSymbol;
	}

	@Override
	protected String getPrintableObjectProperties() {
		return ' ' + name;
	}

	@Override
	public String toString() {
		return name;
//		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).setExcludeFieldNames("useList", "usedByList", "externalSymbols", "internalSymbols", "shadowingSymbols").toString();
	}
}
