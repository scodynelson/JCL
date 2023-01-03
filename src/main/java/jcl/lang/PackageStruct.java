package jcl.lang;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import jcl.lang.condition.exception.PackageErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.PackageStructImpl;
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
	 * Getter for 'used-by' PackageStruct objects.
	 *
	 * @return used-by PackageStruct objects
	 */
	List<PackageStruct> getUsedByList();

	/**
	 * Returns the {@link Collection} of internal symbols in the package.
	 *
	 * @return the {@link Collection} of internal symbols in the package
	 */
	Collection<SymbolStruct> getInternalSymbols();

	/**
	 * Returns a possible external symbol if one exists.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return a possible external symbol if one exists
	 */
	Optional<SymbolStruct> findExternalSymbol(final String symbolName);

	/**
	 * Returns the {@link Collection} of external symbols in the package.
	 *
	 * @return the {@link Collection} of external symbols in the package
	 */
	Collection<SymbolStruct> getExternalSymbols();

	/**
	 * Returns a possible shadowed symbol if one exists.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return a possible shadowed symbol if one exists
	 */
	Optional<SymbolStruct> findShadowedSymbol(final String symbolName);

	/**
	 * Returns a possible accessible symbol if one exists.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return a possible accessible symbol if one exists
	 */
	Optional<SymbolStruct> findAccessibleSymbol(final String symbolName);

	/*
	PACKAGE-STRUCT
	 */

	/**
	 * Returns the string that names package, or nil if the package designator is a package object that has no name
	 * (aka, the package has been deleted).
	 *
	 * @return the package name or nil
	 */
	LispStruct packageName();

	/**
	 * Returns the list of nickname strings for package, not including the name of package.
	 *
	 * @return the list of nickname strings for package
	 */
	ListStruct packageNicknames();

	/**
	 * Returns a list of symbols that have been declared as shadowing symbols in package by shadow or shadowing-import.
	 *
	 * @return a list of symbols that have been declared as shadowing symbols
	 */
	ListStruct packageShadowingSymbols();

	/**
	 * Returns a list of other packages used by package.
	 *
	 * @return a list of other packages used by package
	 */
	ListStruct packageUseList();

	/**
	 * Returns a list of other packages that use package.
	 *
	 * @return a list of other packages that use package
	 */
	ListStruct packageUsedByList();

	/**
	 * Replaces the name and nicknames of package. The old name and all of the old nicknames of package are eliminated
	 * and are replaced by new-name and new-nicknames.
	 *
	 * @param newName
	 * 		the new package name
	 * @param newNicknames
	 * 		the new package nicknames
	 *
	 * @return the package
	 */
	PackageStruct renamePackage(final StringStruct newName, final ListStruct newNicknames);

	/**
	 * Deletes package from all package system data structures. If the operation is successful, delete-package returns
	 * true, otherwise nil. The effect of delete-package is that the name and nicknames of package cease to be
	 * recognized package names.
	 *
	 * @return true if the package was deleted; false otherwise
	 */
	BooleanStruct deletePackage();

	/**
	 * Causes package to inherit all the external symbols of {@code packageToUse}. The inherited symbols become
	 * accessible as internal symbols of package.
	 *
	 * @param packageToUse
	 * 		the package that will be used
	 */
	void usePackage(final PackageStruct packageToUse);

	/**
	 * Causes package to inherit all the external symbols of {@code packagesToUse}. The inherited symbols become
	 * accessible as internal symbols of package.
	 *
	 * @param packagesToUse
	 * 		the packages that will be used
	 *
	 * @return true
	 */
	BooleanStruct usePackage(final ListStruct packagesToUse);

	/**
	 * Causes package to cease inheriting all the external symbols of {@code packageToUnUse}.
	 *
	 * @param packageToUnUse
	 * 		the package that will be un-used
	 */
	void unUsePackage(final PackageStruct packageToUnUse);

	/**
	 * Causes package to cease inheriting all the external symbols of {@code packagesToUnUse}.
	 *
	 * @param packagesToUnUse
	 * 		the packages that will be un-used
	 *
	 * @return true
	 */
	BooleanStruct unUsePackage(final ListStruct packagesToUnUse);

	/**
	 * Locates a symbol whose name is {@code symbolName} in a package.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the found symbol
	 */
	PackageSymbolStruct findSymbol(final String symbolName);

	/**
	 * Locates a symbol whose name is {@code symbolName} in a package.
	 *
	 * @param symbolName
	 * 		the name of the symbol to find
	 *
	 * @return the found symbol
	 */
	default PackageSymbolStruct findSymbol(final StringStruct symbolName) {
		return findSymbol(symbolName.toJavaString());
	}

	/**
	 * Adds symbol to the internals of package, checking for name conflicts with existing symbols either present in
	 * package or accessible to it.
	 *
	 * @param symbol
	 * 		the symbol to import into the package
	 */
	void importSymbol(final SymbolStruct symbol);

	/**
	 * Adds symbols to the internals of package, checking for name conflicts with existing symbols either present in
	 * package or accessible to it.
	 *
	 * @param symbols
	 * 		the symbols to import into the package
	 *
	 * @return true
	 */
	BooleanStruct importSymbols(final ListStruct symbols);

	/**
	 * Inserts the symbols into package as an internal symbol, regardless of whether another symbol of the same name is
	 * shadowed by this action. If a different symbol of the same name is already present in package, that symbol is
	 * first uninterned from package. The new symbol is added to package's shadowing-symbols list.
	 *
	 * @param symbol
	 * 		the symbol to shadow import into the package
	 */
	void shadowingImport(final SymbolStruct symbol);

	/**
	 * Inserts each of symbols into package as an internal symbol, regardless of whether another symbol of the same name
	 * is shadowed by this action. If a different symbol of the same name is already present in package, that symbol is
	 * first uninterned from package. The new symbol is added to package's shadowing-symbols list.
	 *
	 * @param symbols
	 * 		the symbols to shadow import into the package
	 *
	 * @return true
	 */
	BooleanStruct shadowingImport(final ListStruct symbols);

	/**
	 * Makes the symbol that is accessible in package (whether directly or by inheritance) be an external symbol of that
	 * package.
	 *
	 * @param symbol
	 * 		the symbol to export
	 */
	void export(final SymbolStruct symbol);

	/**
	 * Makes each symbol that is accessible in package (whether directly or by inheritance) be an external symbol of
	 * that package.
	 *
	 * @param symbols
	 * 		the symbols to export
	 *
	 * @return true
	 */
	BooleanStruct export(final ListStruct symbols);

	/**
	 * Reverts an external symbol in package to internal status.
	 *
	 * @param symbol
	 * 		the symbol to un-export
	 */
	void unexport(final SymbolStruct symbol);

	/**
	 * Reverts external symbols in package to internal status.
	 *
	 * @param symbols
	 * 		the symbols to un-export
	 *
	 * @return true
	 */
	BooleanStruct unexport(final ListStruct symbols);

	/**
	 * Assures that the symbol with the name given by {@code symbolName} is present in the package. If the symbol is
	 * not present in package (directly, not by inheritance), then a corresponding symbol is created with that name,
	 * and inserted into package as an internal symbol. The corresponding symbol, whether pre-existing or newly created,
	 * is then added, if not already present, to the shadowing symbols list of package.
	 *
	 * @param symbolName
	 * 		the name of the symbols to shadow
	 */
	void shadow(final StringStruct symbolName);

	/**
	 * Assures that symbols with names given by {@code symbolNames} are present in the package. If the symbol is not
	 * present in package (directly, not by inheritance), then a corresponding symbol is created with that name, and
	 * inserted into package as an internal symbol. The corresponding symbol, whether pre-existing or newly created, is
	 * then added, if not already present, to the shadowing symbols list of package.
	 *
	 * @param symbolNames
	 * 		the names of the symbols to shadow
	 *
	 * @return true
	 */
	BooleanStruct shadow(final ListStruct symbolNames);

	/**
	 * Enters a symbol named string into package. If a symbol whose name is the same as {@code symbolName} is already
	 * accessible in package, it is returned. If no such symbol is accessible in package, a new symbol with the given
	 * name is created and entered into package.
	 *
	 * @param symbolName
	 * 		the name of the symbol to intern
	 *
	 * @return the symbol if found, or a new symbol if not found
	 */
	PackageSymbolStruct intern(final String symbolName);

	/**
	 * Enters a symbol named string into package. If a symbol whose name is the same as {@code symbolName} is already
	 * accessible in package, it is returned. If no such symbol is accessible in package, a new symbol with the given
	 * name is created and entered into package.
	 *
	 * @param symbolName
	 * 		the name of the symbol to intern
	 *
	 * @return the symbol if found, or a new symbol if not found
	 */
	default PackageSymbolStruct intern(final StringStruct symbolName) {
		return intern(symbolName.toJavaString());
	}

	/**
	 * Removes {@code symbol} from package.
	 *
	 * @param symbol
	 * 		the symbol to un-intern from the package
	 *
	 * @return true if the symbol was removed; false otherwise
	 */
	BooleanStruct unintern(final SymbolStruct symbol);

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
		final PackageStruct struct = GlobalPackageStruct.ALL_PACKAGES.get(packageName);
		if (struct == null) {
			throw new PackageErrorException(packageName + " is not the name of a package.", null);
		}
		return struct;
	}

	/**
	 * Finds the matching package in the {@link GlobalPackageStruct#ALL_PACKAGES} map by the provided {@code
	 * packageName}.
	 *
	 * @param packageDesignator
	 * 		the name of the package to location within the {@link GlobalPackageStruct#ALL_PACKAGES} map
	 *
	 * @return the located package for the provided {@code packageName}
	 */
	static LispStruct findPackage(final LispStruct packageDesignator) {
		try {
			return fromDesignator(packageDesignator);
		} catch (final PackageErrorException ignore) {
			return NILStruct.INSTANCE;
		}
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
	static ListStruct findAllSymbols(final StringStruct symbolName) {
		ListStruct result = NILStruct.INSTANCE;
		for (final PackageStruct pkg : GlobalPackageStruct.ALL_PACKAGES.values()) {
			final PackageSymbolStruct symbol = pkg.findSymbol(symbolName);
			if (symbol.found()) {
				final SymbolStruct foundSymbol = symbol.getSymbol();
				result = ConsStruct.toLispCons(foundSymbol, result);
			}
		}
		return result;
	}

	/**
	 * Creates a new PackageStruct with the name {@code name} and adds it to the {@link
	 * GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @param name
	 * 		the name of the new package
	 *
	 * @return a new PackageStruct instance
	 */
	static PackageStruct toLispPackage(final String name) {
		return toLispPackage(name, Collections.emptyList());
	}

	/**
	 * Creates a new PackageStruct with the name {@code name} and nicknames {@code nicknames}, and adds it to the {@link
	 * GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @param name
	 * 		the name of the new package
	 * @param nicknames
	 * 		the nicknames of the new package
	 *
	 * @return a new PackageStruct instance
	 */
	static PackageStruct toLispPackage(final String name, final List<String> nicknames) {
		if (GlobalPackageStruct.ALL_PACKAGES.containsKey(name)) {
			throw new ProgramErrorException("Package name " + name + " is already in use.");
		}
		for (final String nickname : nicknames) {
			if (GlobalPackageStruct.ALL_PACKAGES.containsKey(nickname)) {
				throw new ProgramErrorException("Package name " + nickname + " is already in use.");
			}
		}

		final PackageStructImpl struct = new PackageStructImpl(name, nicknames);
		GlobalPackageStruct.ALL_PACKAGES.put(name, struct);
		for (final String nickname : nicknames) {
			GlobalPackageStruct.ALL_PACKAGES.put(nickname, struct);
		}
		return struct;
	}

	/**
	 * Returns the provided {@link LispStruct} as a PackageStruct.
	 * <p>
	 * This method does NOT create a new package, only attempts to locate existing packages.
	 *
	 * @param struct
	 * 		the structure to represent as a PackageStruct
	 *
	 * @return the provided {@link LispStruct} as a PackageStruct
	 *
	 * @throws TypeErrorException
	 * 		if the provided {@code struct} is not a valid package-designator (aka, a PackageStruct, CharacterStruct,
	 * 		SymbolStruct, or StringStruct)
	 */
	static PackageStruct fromDesignator(final LispStruct struct) {
		if (struct instanceof PackageStruct) {
			return (PackageStruct) struct;
		} else if (struct instanceof SymbolStruct) {
			final SymbolStruct symbolStruct = (SymbolStruct) struct;
			final String name = symbolStruct.getName();
			return findPackage(name);
		} else if (struct instanceof CharacterStruct) {
			final CharacterStruct characterStruct = (CharacterStruct) struct;
			final String packageName = characterStruct.toJavaCharacter().toString();
			return findPackage(packageName);
		} else if (struct instanceof StringStruct) {
			final StringStruct stringStruct = (StringStruct) struct;
			final String packageName = stringStruct.toJavaString();
			return findPackage(packageName);
		} else {
			throw new TypeErrorException("Type cannot be converted to PACKAGE.");
		}
	}

	/**
	 * Creates a new PackageStruct with the name {@code name} and nicknames {@code nicknames}, and adds it to the
	 * {@link GlobalPackageStruct#ALL_PACKAGES} map. The resulting PackageStruct will also use the packages in {@code
	 * usePackages}.
	 *
	 * @param packageName
	 * 		the name of the new package
	 * @param nicknames
	 * 		the nicknames of the new package
	 * @param usePackages
	 * 		the packages the new package will use
	 *
	 * @return a new PackageStruct instance
	 */
	static PackageStruct makePackage(final StringStruct packageName, final ListStruct nicknames,
	                                 final ListStruct usePackages) {
		final String name = packageName.toJavaString();
		final List<String> realNicknames
				= nicknames.stream()
				           .map(StringStruct.class::cast)
				           .map(StringStruct::toJavaString)
				           .collect(Collectors.toList());

		final PackageStruct struct = toLispPackage(name, realNicknames);
		struct.usePackage(usePackages);
		return struct;
	}

	/**
	 * Lists all current packages existent in the {@link GlobalPackageStruct#ALL_PACKAGES} map.
	 *
	 * @return a list of all current packages existent in the {@link GlobalPackageStruct#ALL_PACKAGES} map
	 */
	static ListStruct listAllPackages() {
		ListStruct result = NILStruct.INSTANCE;
		for (final PackageStruct pkg : GlobalPackageStruct.ALL_PACKAGES.values()) {
			result = ConsStruct.toLispCons(pkg, result);
		}
		return result;
	}
}
