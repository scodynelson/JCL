package jcl.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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

	default LispStruct packageName() {
		final String name = getName();
		return (name == null) ? NILStruct.INSTANCE : StringStruct.toLispString(name);
	}

	/**
	 * Getter for nicknames property.
	 *
	 * @return nicknames property
	 */
	List<String> getNicknames();

	default ListStruct packageNicknames() {
		final List<String> nicknames = getNicknames();
		final List<LispStruct> nicknamesStructs =
				nicknames.stream()
				         .map(StringStruct::toLispString)
				         .collect(Collectors.toList());
		return ListStruct.toLispList(nicknamesStructs);
	}

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

	default ListStruct packageShadowingSymbols() {
		final Collection<SymbolStruct> shadowingSymbols = getShadowingSymbols().values();
		return ListStruct.toLispList(new ArrayList<>(shadowingSymbols));
	}

	/**
	 * Getter for 'use' PackageStruct objects.
	 *
	 * @return used-by PackageStruct objects
	 */
	List<PackageStruct> getUseList();

	default ListStruct packageUseList() {
		final List<PackageStruct> useList = getUseList();
		return ListStruct.toLispList(useList);
	}

	/**
	 * Getter for 'used-by' PackageStruct objects.
	 *
	 * @return used-by PackageStruct objects
	 */
	List<PackageStruct> getUsedByList();

	default ListStruct packageUsedByList() {
		final List<PackageStruct> usedByList = getUsedByList();
		return ListStruct.toLispList(usedByList);
	}

	/**
	 * Renames the package and updates it in the {@link GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @param newName
	 * 		the new package name
	 */
	void renamePackage(final String newName);

	default PackageStruct renamePackage(final StringStruct newName, final ListStruct nicknames) {
		final List<String> newNicknames
				= nicknames.stream()
				           .map(StringStruct.class::cast)
				           .map(StringStruct::toJavaString)
				           .collect(Collectors.toList());
		renamePackage(newName.toJavaString(), newNicknames);
		return this;
	}

	/**
	 * Renames the package and updates it in the global ALL_PACKAGES map.
	 *
	 * @param newName
	 * 		the new package name
	 * @param newNicknames
	 * 		the new package nicknames
	 */
	void renamePackage(final String newName, final List<String> newNicknames);

	default BooleanStruct deletePackage1() { // TODO: fix method name

		if (getName() == null) {
			return NILStruct.INSTANCE;
		}

		deletePackage();
		return TStruct.INSTANCE;
	}

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

	default BooleanStruct usePackage(final ListStruct packagesToUse) {
		final PackageStruct[] newPackagesToUse
				= packagesToUse.stream()
				               .map(PackageStruct.class::cast)
				               .toArray(PackageStruct[]::new);
		usePackage(newPackagesToUse);
		return TStruct.INSTANCE;
	}

	/**
	 * Updates the package to un-use the provided {@code packagesToUnUse}.
	 *
	 * @param packagesToUnUse
	 * 		the packages that will be un-used
	 */
	void unUsePackage(final PackageStruct... packagesToUnUse);

	default BooleanStruct unusePackage(final ListStruct packagesToUnUse) {
		final PackageStruct[] newPackagesToUnUse
				= packagesToUnUse.stream()
				                 .map(PackageStruct.class::cast)
				                 .toArray(PackageStruct[]::new);
		unUsePackage(newPackagesToUnUse);
		return TStruct.INSTANCE;
	}

	/**
	 * Locates the symbol matching the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the symbol if found and it's package location type, or null if not found
	 */
	PackageSymbolStruct findSymbol(final String symbolName);

	default LispStruct findSymbol(final StringStruct symbolName) {
		final PackageSymbolStruct packageSymbol = findSymbol(symbolName.toJavaString());
		if (packageSymbol == null) {
			return ValuesStruct.valueOf(NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		final SymbolStruct symbol = packageSymbol.getSymbol();
		final KeywordStruct packageSymbolType = packageSymbol.getPackageSymbolType();
		return ValuesStruct.valueOf(symbol, packageSymbolType);
	}

	/**
	 * Imports the provided {@code symbols} into the package.
	 *
	 * @param symbols
	 * 		the symbols to import into the package
	 */
	void importSymbols(final SymbolStruct... symbols);

	default BooleanStruct importSymbols(final ListStruct symbols) {
		final SymbolStruct[] newSymbols
				= symbols.stream()
				         .map(SymbolStruct.class::cast)
				         .toArray(SymbolStruct[]::new);
		importSymbols(newSymbols);
		return TStruct.INSTANCE;
	}

	/**
	 * Performs a shadowing import of the provided {@code symbols} into the package, shadowing each one and uninterning
	 * current symbols with matching symbol names that already exist in the internal symbols of the package.
	 *
	 * @param symbols
	 * 		the symbols to shadow import into the package
	 */
	void shadowingImport(final SymbolStruct... symbols);

	default BooleanStruct shadowingImport(final ListStruct symbols) {
		final SymbolStruct[] newSymbols
				= symbols.stream()
				         .map(SymbolStruct.class::cast)
				         .toArray(SymbolStruct[]::new);
		shadowingImport(newSymbols);
		return TStruct.INSTANCE;
	}

	/**
	 * Exports the provided symbols and puts them into the externalSymbols map. All found symbols are exported and
	 * those
	 * not found are stored for throwing in a {@link PackageErrorException}.
	 *
	 * @param symbols
	 * 		the symbols to export
	 */
	void export(final SymbolStruct... symbols);

	default BooleanStruct export(final ListStruct symbols) {
		final SymbolStruct[] newSymbols
				= symbols.stream()
				         .map(SymbolStruct.class::cast)
				         .toArray(SymbolStruct[]::new);
		export(newSymbols);
		return TStruct.INSTANCE;
	}

	/**
	 * Un-exports the provided symbols and removes them from the externalSymbols map. All found symbols are un-exported
	 * and those not found are stored for throwing in a {@link PackageErrorException}.
	 *
	 * @param symbols
	 * 		the symbols to un-export
	 */
	void unexport(final SymbolStruct... symbols);

	default BooleanStruct unexport(final ListStruct symbols) {
		final SymbolStruct[] newSymbols
				= symbols.stream()
				         .map(SymbolStruct.class::cast)
				         .toArray(SymbolStruct[]::new);
		unexport(newSymbols);
		return TStruct.INSTANCE;
	}

	/**
	 * Shadows the provided {@code symbolNames}, either by finding the current non-inherited matching symbol, or by
	 * creating a new symbol with the non-existent symbolName.
	 *
	 * @param symbolNames
	 * 		the names of the symbols to shadow
	 */
	void shadow(final String... symbolNames);

	default LispStruct shadow(final ListStruct symbolNames) {
		final String[] newSymbolNames
				= symbolNames.stream()
				             .map(StringStruct.class::cast)
				             .map(StringStruct::toJavaString)
				             .toArray(String[]::new);
		shadow(newSymbolNames);
		return TStruct.INSTANCE;
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
	PackageSymbolStruct intern(final String symbolName);

	default LispStruct intern(final StringStruct symbolName) {
		final PackageSymbolStruct packageSymbol = intern(symbolName.toJavaString());
		if (packageSymbol == null) {
			return ValuesStruct.valueOf(NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		final SymbolStruct symbol = packageSymbol.getSymbol();
		final KeywordStruct packageSymbolType = packageSymbol.getPackageSymbolType();
		return ValuesStruct.valueOf(symbol, packageSymbolType);
	}

	/**
	 * Un-interns the provided {@code symbol} from the package.
	 *
	 * @param symbol
	 * 		the symbol to un-intern from the package
	 *
	 * @return whether a symbol was indeed un-interned or not
	 */
	boolean unintern(final SymbolStruct symbol);

	default BooleanStruct unintern1(final SymbolStruct symbol) { // TODO: fix method name
		final boolean wasUninterned = unintern(symbol);
		return wasUninterned ? TStruct.INSTANCE : NILStruct.INSTANCE;
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

	static ListStruct findAllSymbols1(final StringStruct symbolName) { // TODO: fix method name
		return ListStruct.toLispList(findAllSymbols(symbolName.toJavaString()));
	}

	/**
	 * Lists all current packages existent in the {@link GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @return a list of all current packages existent in the {@link GlobalPackageStruct#ALL_PACKAGES} map
	 */
	static List<PackageStruct> listAllPackages() {
		return new ArrayList<>(GlobalPackageStruct.ALL_PACKAGES.values());
	}

	static ListStruct listAllPackages1() { // TODO: fix method name
		return ListStruct.toLispList(listAllPackages());
	}
}
