/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

import jcl.lang.condition.exception.PackageErrorException;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.type.PackageType;

/**
 * The {@link PackageStruct} is the object representation of a Lisp 'package' type.
 */
public class PackageStruct extends BuiltInClassStruct {

	public static final KeywordStructImpl INTERNAL_KEYWORD = KeywordStructImpl.valueOf("INTERNAL");
	public static final KeywordStructImpl EXTERNAL_KEYWORD = KeywordStructImpl.valueOf("EXTERNAL");
	public static final KeywordStructImpl INHERITED_KEYWORD = KeywordStructImpl.valueOf("INHERITED");

	/**
	 * The {@link List} of {@link PackageStruct}s that the package uses.
	 */
	private final List<PackageStruct> useList = new ArrayList<>();

	/**
	 * The {@link List} of {@link PackageStruct}s that the package is used by.
	 */
	private final List<PackageStruct> usedByList = new ArrayList<>();

	/**
	 * The {@link Map} of the packages external {@link SymbolStructImpl}s.
	 * NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols (aka. anything in ExternalSymbols or
	 * ShadowingSymbols are in InternalSymbols but not vice-versa)
	 */
	protected final Map<String, SymbolStructImpl> externalSymbols = new ConcurrentHashMap<>();

	/**
	 * The {@link Map} of the packages internal {@link SymbolStructImpl}s.
	 */
	private final Map<String, SymbolStructImpl> internalSymbols = new ConcurrentHashMap<>();

	/**
	 * The {@link Map} of the packages shadowing {@link SymbolStructImpl}s.
	 * NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols (aka. anything in ExternalSymbols or
	 * ShadowingSymbols are in InternalSymbols but not vice-versa)
	 */
	private final Map<String, SymbolStructImpl> shadowingSymbols = new ConcurrentHashMap<>();

	/**
	 * The name of package.
	 */
	private String name;

	/**
	 * The {@link List} of nicknames of the package.
	 */
	private List<String> nicknames;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 */
	protected PackageStruct(final String name) {
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
	private PackageStruct(final String name, final List<String> nicknames) {
		this(name, nicknames, new ArrayList<>());
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
	private PackageStruct(final String name, final List<String> nicknames, final PackageStruct... useList) {
		this(name, nicknames, new ArrayList<>(Arrays.asList(useList)));
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
	private PackageStruct(final String name, final List<String> nicknames, final List<PackageStruct> useList) {
		super(PackageType.INSTANCE, null, null);
		this.name = name;
		this.nicknames = nicknames;

		this.useList.addAll(useList);
		final PackageStruct[] useListArray = new PackageStruct[useList.size()];
		internalUsePackage(useList.toArray(useListArray));

		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		GlobalPackageStruct.ALL_PACKAGES.put(name, this);
		for (final String nickname : nicknames) {
			GlobalPackageStruct.ALL_PACKAGES.put(nickname, this);
		}
	}

	public static PackageStruct valueOf(final String name) {
		return new PackageStruct(name);
	}

	public static PackageStruct valueOf(final String name, final List<String> nicknames) {
		return new PackageStruct(name, nicknames);
	}

	public static PackageStruct valueOf(final String name, final List<String> nicknames, final PackageStruct... useList) {
		return new PackageStruct(name, nicknames, useList);
	}

	public static PackageStruct valueOf(final String name, final List<String> nicknames, final List<PackageStruct> useList) {
		return new PackageStruct(name, nicknames, useList);
	}

	@Override
	public Supplier<PackageStruct> asPackage() {
		return () -> this;
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
	public Map<String, SymbolStructImpl> getExternalSymbols() {
		return externalSymbols;
	}

	/**
	 * Getter for package {@link #shadowingSymbols} property.
	 *
	 * @return package {@link #shadowingSymbols} property
	 */
	public Map<String, SymbolStructImpl> getShadowingSymbols() {
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
		renamePackage(newName, Collections.emptyList());
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
		GlobalPackageStruct.ALL_PACKAGES.put(newName, this);

		nicknames.forEach(GlobalPackageStruct.ALL_PACKAGES::remove);
		nicknames = newNicknames;
		for (final String nickname : newNicknames) {
			GlobalPackageStruct.ALL_PACKAGES.put(nickname, this);
		}
	}

	/**
	 * Deletes the package and removes it from the {@link GlobalPackageStruct#ALL_PACKAGES} map. The deletion method
	 * does not destroy the object, but clears its global usages and changes the name to 'null'.
	 */
	public void deletePackage() {
		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		nicknames.forEach(GlobalPackageStruct.ALL_PACKAGES::remove);
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
				throw new PackageErrorException(this + " can't use package " + GlobalPackageStruct.KEYWORD, this);
			}

			// Shadow current nonInherited symbols
			for (final String symbolName : packageToUse.externalSymbols.keySet()) {
				final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
				if (nonInheritedPackageSymbol != null) {
					final SymbolStructImpl nonInheritedSymbol = nonInheritedPackageSymbol.getSymbol();
					shadowingSymbols.put(symbolName, nonInheritedSymbol);
				}
			}

			packageToUse.usedByList.add(this);
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

		final SymbolStructImpl foundSymbol = findInheritedSymbol(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, INHERITED_KEYWORD);
		}

		return null;
	}

	/**
	 * Imports the provided {@code symbols} into the package.
	 *
	 * @param symbols
	 * 		the symbols to import into the package
	 */
	public void importSymbols(final SymbolStructImpl... symbols) {
		for (final SymbolStructImpl symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
			if (nonInheritedPackageSymbol != null) {
				continue;
			}

			internalSymbols.put(symbolName, symbol);

			final SymbolStructImpl foundSymbol = findInheritedSymbol(symbolName);
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
	public void shadowingImport(final SymbolStructImpl... symbols) {
		for (final SymbolStructImpl symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct nonInheritedPackageSymbol = findNonInheritedSymbol(symbolName);
			if (nonInheritedPackageSymbol != null) {
				final SymbolStructImpl nonInheritedSymbol = nonInheritedPackageSymbol.getSymbol();
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
	public void export(final SymbolStructImpl... symbols) {
		final List<String> notFoundSymbolNames = new ArrayList<>();

		for (final SymbolStructImpl symbol : symbols) {
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

		handleExportSymbolsNotInPackageError(notFoundSymbolNames);
	}

	/**
	 * Un-exports the provided symbols and removes them from the externalSymbols map. All found symbols are un-exported
	 * and those not found are stored for throwing in a {@link PackageErrorException}.
	 *
	 * @param symbols
	 * 		the symbols to un-export
	 */
	public void unexport(final SymbolStructImpl... symbols) {
		final List<String> notFoundSymbolNames = new ArrayList<>();

		for (final SymbolStructImpl symbol : symbols) {
			final String symbolName = symbol.getName();

			final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
			if (foundPackageSymbol == null) {
				notFoundSymbolNames.add(symbolName);
				continue;
			}

			externalSymbols.remove(symbolName);
		}

		handleExportSymbolsNotInPackageError(notFoundSymbolNames);
	}

	/**
	 * Handles the building of the {@link PackageErrorException} when symbols are not accessible in the package and
	 * cannot be exported or unexported.
	 *
	 * @param notFoundSymbolNames
	 * 		the symbols not accessible in the package
	 */
	private void handleExportSymbolsNotInPackageError(final List<String> notFoundSymbolNames) {
		if (!notFoundSymbolNames.isEmpty()) {
			final StringBuilder exceptionStringBuilder
					= new StringBuilder("The following symbols are not accessible in package " + this + ": (");
			for (final String notFoundSymbolName : notFoundSymbolNames) {
				exceptionStringBuilder.append(notFoundSymbolName);
				exceptionStringBuilder.append(' ');
			}
			exceptionStringBuilder.append(')');
			throw new PackageErrorException(exceptionStringBuilder.toString(), this);
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

			final SymbolStructImpl nonInheritedSymbol;
			if (nonInheritedPackageSymbol == null) {
				nonInheritedSymbol = SymbolStructImpl.valueOf(symbolName);
				internalSymbols.put(symbolName, nonInheritedSymbol);
				nonInheritedSymbol.setSymbolPackage(this);
			} else {
				nonInheritedSymbol = nonInheritedPackageSymbol.getSymbol();
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

		final SymbolStructImpl symbolStruct = SymbolStructImpl.valueOf(symbolName);
		internalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, INTERNAL_KEYWORD);
	}

	/**
	 * Un-interns the provided {@code symbol} from the package.
	 *
	 * @param symbol
	 * 		the symbol to un-intern from the package
	 *
	 * @return whether a symbol was indeed un-interned or not
	 */
	public boolean unintern(final SymbolStructImpl symbol) {
		final String symbolName = symbol.getName();

		// Test for conflicts BEFORE we remove anything
		final Set<SymbolStructImpl> shadowingConflicts = getShadowingConflicts(symbolName);
		if (shadowingConflicts.size() > 1) {
			final StringBuilder exceptionStringBuilder
					= new StringBuilder("Uninterning " + symbolName + " from " + this + " would cause conflicts among : (");
			for (final SymbolStructImpl conflictingSymbol : shadowingConflicts) {
				exceptionStringBuilder.append(conflictingSymbol);
				exceptionStringBuilder.append(' ');
			}
			exceptionStringBuilder.append(')');
			throw new PackageErrorException(exceptionStringBuilder.toString(), this);
		}

		final SymbolStructImpl externalSymbol = externalSymbols.remove(symbolName);
		final SymbolStructImpl shadowingSymbol = shadowingSymbols.remove(symbolName);
		final SymbolStructImpl internalSymbol = internalSymbols.remove(symbolName);

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
	public static List<SymbolStructImpl> findAllSymbols(final String symbolName) {
		final Set<SymbolStructImpl> allSymbols = new HashSet<>();
		for (final PackageStruct packageStruct : GlobalPackageStruct.ALL_PACKAGES.values()) {
			final PackageSymbolStruct foundPackageSymbol = packageStruct.findSymbol(symbolName);
			if (foundPackageSymbol != null) {
				final SymbolStructImpl foundSymbol = foundPackageSymbol.getSymbol();
				allSymbols.add(foundSymbol);
			}
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
	private Set<SymbolStructImpl> getShadowingConflicts(final String symbolName) {
		if (!shadowingSymbols.containsKey(symbolName)) {
			return Collections.emptySet();
		}

		final Set<SymbolStructImpl> conflictingInheritedSymbols = new HashSet<>();
		for (final PackageStruct usedPackage : useList) {
			final PackageSymbolStruct inheritedPackageSymbol = usedPackage.findSymbol(symbolName);
			if (inheritedPackageSymbol != null) {
				final SymbolStructImpl inheritedSymbol = inheritedPackageSymbol.getSymbol();
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

		SymbolStructImpl foundSymbol = externalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, EXTERNAL_KEYWORD);
		}

		foundSymbol = shadowingSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, INTERNAL_KEYWORD);
		}

		foundSymbol = internalSymbols.get(symbolName);
		if (foundSymbol != null) {
			return new PackageSymbolStruct(foundSymbol, INTERNAL_KEYWORD);
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
	private SymbolStructImpl findInheritedSymbol(final String symbolName) {
		// NOTE: Order matters here!!

		SymbolStructImpl foundSymbol = null;
		for (final PackageStruct usedPackage : useList) {
			final PackageSymbolStruct inheritedPackageSymbol = usedPackage.findSymbol(symbolName);
			if (inheritedPackageSymbol == null) {
				continue;
			}

			final KeywordStructImpl packageSymbolType = inheritedPackageSymbol.getPackageSymbolType();
			if (EXTERNAL_KEYWORD.equals(packageSymbolType)) {
				foundSymbol = inheritedPackageSymbol.getSymbol();
				break;
			}
		}

		return foundSymbol;
	}

	@Override
	public String toString() {
//		final String typeClassName = getType().getClass().getSimpleName().toUpperCase();
		return "#<PACKAGE \"" + name + "\">";
	}
}
