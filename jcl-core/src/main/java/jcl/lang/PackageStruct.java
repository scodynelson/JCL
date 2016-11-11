package jcl.lang;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.lang.condition.exception.PackageErrorException;
import jcl.lang.statics.GlobalPackageStruct;

/**
 * The {@link PackageStruct} is the object representation of a Lisp 'package' type.
 */
public interface PackageStruct extends LispStruct {

	/**
	 * Getter for name property.
	 *
	 * @return name property
	 */
	String getName();

	/**
	 * Getter for nicknames property.
	 *
	 * @return nicknames property
	 */
	List<String> getNicknames();

	/**
	 * Getter for external-symbols property.
	 *
	 * @return external-symbols property
	 */
	Map<String, SymbolStruct> getExternalSymbols();

	/**
	 * Getter for shadowing-symbols property.
	 *
	 * @return shadowing-symbols property
	 */
	Map<String, SymbolStruct> getShadowingSymbols();

	/**
	 * Getter for 'use' PackageStruct objects.
	 *
	 * @return used-by PackageStruct objects
	 */
	List<PackageStruct> getUseList();

	/**
	 * Getter for 'used-by' PackageStruct objects.
	 *
	 * @return used-by PackageStruct objects
	 */
	List<PackageStruct> getUsedByList();

	/**
	 * Renames the package and updates it in the {@link GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @param newName
	 * 		the new package name
	 */
	void renamePackage(final String newName);

	/**
	 * Renames the package and updates it in the global ALL_PACKAGES map.
	 *
	 * @param newName
	 * 		the new package name
	 * @param newNicknames
	 * 		the new package nicknames
	 */
	void renamePackage(final String newName, final List<String> newNicknames);

	/**
	 * Deletes the package and removes it from the {@link GlobalPackageStruct#ALL_PACKAGES} map. The deletion method
	 * does not destroy the object, but clears its global usages and changes the name to 'null'.
	 */
	void deletePackage();

	/**
	 * Updates the package to use the provided {@code packagesToUse}.
	 *
	 * @param packagesToUse
	 * 		the packages that will be used
	 */
	void usePackage(final PackageStruct... packagesToUse);

	/**
	 * Updates the package to un-use the provided {@code packagesToUnUse}.
	 *
	 * @param packagesToUnUse
	 * 		the packages that will be un-used
	 */
	void unUsePackage(final PackageStruct... packagesToUnUse);

	/**
	 * Locates the symbol matching the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the symbol if found and it's package location type, or null if not found
	 */
	PackageSymbolStruct findSymbol(final String symbolName);

	/**
	 * Imports the provided {@code symbols} into the package.
	 *
	 * @param symbols
	 * 		the symbols to import into the package
	 */
	void importSymbols(final SymbolStruct... symbols);

	/**
	 * Performs a shadowing import of the provided {@code symbols} into the package, shadowing each one and uninterning
	 * current symbols with matching symbol names that already exist in the internal symbols of the package.
	 *
	 * @param symbols
	 * 		the symbols to shadow import into the package
	 */
	void shadowingImport(final SymbolStruct... symbols);

	/**
	 * Exports the provided symbols and puts them into the externalSymbols map. All found symbols are exported and
	 * those
	 * not found are stored for throwing in a {@link PackageErrorException}.
	 *
	 * @param symbols
	 * 		the symbols to export
	 */
	void export(final SymbolStruct... symbols);

	/**
	 * Un-exports the provided symbols and removes them from the externalSymbols map. All found symbols are un-exported
	 * and those not found are stored for throwing in a {@link PackageErrorException}.
	 *
	 * @param symbols
	 * 		the symbols to un-export
	 */
	void unexport(final SymbolStruct... symbols);

	/**
	 * Shadows the provided {@code symbolNames}, either by finding the current non-inherited matching symbol, or by
	 * creating a new symbol with the non-existent symbolName.
	 *
	 * @param symbolNames
	 * 		the names of the symbols to shadow
	 */
	void shadow(final String... symbolNames);

	/**
	 * Locates the symbol matching the provided {@code symbolName} or creates a new internal symbol with it, interns it
	 * into the package, and returns it with it's package location type.
	 *
	 * @param symbolName
	 * 		the name of the symbol to intern
	 *
	 * @return the symbol if found and it's package location type, or a new symbol with internal type if not found
	 */
	PackageSymbolStruct intern(final String symbolName);

	/**
	 * Un-interns the provided {@code symbol} from the package.
	 *
	 * @param symbol
	 * 		the symbol to un-intern from the package
	 *
	 * @return whether a symbol was indeed un-interned or not
	 */
	boolean unintern(final SymbolStruct symbol);

	/**
	 * Finds the matching package in the {@link GlobalPackageStruct#ALL_PACKAGES} map by the provided {@code
	 * packageName}.
	 *
	 * @param packageName
	 * 		the name of the package to location within the {@link GlobalPackageStruct#ALL_PACKAGES} map
	 *
	 * @return the located package for the provided {@code packageName}
	 */
	static PackageStruct findPackage(final String packageName) {
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
	static List<SymbolStruct> findAllSymbols(final String symbolName) {
		final Set<SymbolStruct> allSymbols = new HashSet<>();
		for (final PackageStruct packageStruct : GlobalPackageStruct.ALL_PACKAGES.values()) {
			final PackageSymbolStruct foundPackageSymbol = packageStruct.findSymbol(symbolName);
			if (foundPackageSymbol != null) {
				final SymbolStruct foundSymbol = foundPackageSymbol.getSymbol();
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
	static List<PackageStruct> listAllPackages() {
		return new ArrayList<>(GlobalPackageStruct.ALL_PACKAGES.values());
	}
}
