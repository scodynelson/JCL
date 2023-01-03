/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PackageSymbolStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.PackageErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;

/**
 * The {@link PackageStructImpl} is the object representation of a Lisp 'package' type.
 */
public class PackageStructImpl extends LispStructImpl implements PackageStruct {

	/**
	 * The {@link List} of {@link PackageStruct}s that the package uses.
	 */
	private final List<PackageStruct> useList = new ArrayList<>();

	/**
	 * The {@link List} of {@link PackageStruct}s that the package is used by.
	 */
	private final List<PackageStruct> usedByList = new ArrayList<>();

	/**
	 * The {@link Map} of the packages external {@link SymbolStruct}s.
	 * <p>
	 * NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols (aka. anything in ExternalSymbols or
	 * ShadowingSymbols are in InternalSymbols but not vice-versa)
	 */
	protected final Map<String, SymbolStruct> externalSymbols = new ConcurrentHashMap<>();

	/**
	 * The {@link Map} of the packages internal {@link SymbolStruct}s.
	 */
	private final Map<String, SymbolStruct> internalSymbols = new ConcurrentHashMap<>();

	/**
	 * The {@link Map} of the packages shadowing {@link SymbolStruct}s.
	 * <p>
	 * NOTE: ExternalSymbols and ShadowingSymbols are subsets of InternalSymbols (aka. anything in ExternalSymbols or
	 * ShadowingSymbols are in InternalSymbols but not vice-versa)
	 */
	private final Map<String, SymbolStruct> shadowingSymbols = new ConcurrentHashMap<>();

	/**
	 * The name of package.
	 */
	private String name;

	/**
	 * The {@link List} of nicknames of the package.
	 */
	private final List<String> nicknames;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the package name
	 */
	public PackageStructImpl(final String name) {
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
	public PackageStructImpl(final String name, final List<String> nicknames) {
		this.name = name;
		this.nicknames = nicknames;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public List<PackageStruct> getUsedByList() {
		return usedByList;
	}

	@Override
	public Collection<SymbolStruct> getInternalSymbols() {
		return internalSymbols.values();
	}

	@Override
	public Optional<SymbolStruct> findExternalSymbol(final String symbolName) {
		return Optional.ofNullable(externalSymbols.get(symbolName));
	}

	@Override
	public Collection<SymbolStruct> getExternalSymbols() {
		return externalSymbols.values();
	}

	@Override
	public Optional<SymbolStruct> findShadowedSymbol(final String symbolName) {
		return Optional.ofNullable(shadowingSymbols.get(symbolName));
	}

	// Returns null if symbol is not accessible in this package.
	@Override
	public Optional<SymbolStruct> findAccessibleSymbol(final String symbolName) {
		// Look in external and internal symbols of this package.
		SymbolStruct symbol = externalSymbols.get(symbolName);
		if (symbol != null) {
			return Optional.of(symbol);
		}
		symbol = internalSymbols.get(symbolName);
		if (symbol != null) {
			return Optional.of(symbol);
		}
		// Look in external symbols of used packages.
		for (final PackageStruct usedPackage : useList) {
			final Optional<SymbolStruct> externalSymbol = usedPackage.findExternalSymbol(symbolName);
			if (externalSymbol.isPresent()) {
				return externalSymbol;
			}
		}
		// Not found.
		return Optional.empty();
	}

	/*
	PACKAGE-STRUCT
	 */

	@Override
	public LispStruct packageName() {
		return (name == null) ? NILStruct.INSTANCE : StringStruct.toLispString(name);
	}

	@Override
	public ListStruct packageNicknames() {
		final List<LispStruct> nicknamesStructs =
				nicknames.stream()
				         .map(StringStruct::toLispString)
				         .collect(Collectors.toList());
		return ListStruct.toLispList(nicknamesStructs);
	}

	@Override
	public ListStruct packageShadowingSymbols() {
		return ListStruct.toLispList(new ArrayList<>(shadowingSymbols.values()));
	}

	@Override
	public ListStruct packageUseList() {
		return ListStruct.toLispList(useList);
	}

	@Override
	public ListStruct packageUsedByList() {
		return ListStruct.toLispList(usedByList);
	}

	@Override
	public PackageStruct renamePackage(final StringStruct newName, final ListStruct newNicknames) {
		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		nicknames.forEach(GlobalPackageStruct.ALL_PACKAGES::remove);

		if (GlobalPackageStruct.ALL_PACKAGES.containsKey(name)) {
			throw new PackageErrorException("A package named " + name + " already exists.", this);
		}
		// TODO: check name and nicknames before modifying things??
		name = newName.toJavaString();
		GlobalPackageStruct.ALL_PACKAGES.put(name, this);

		nicknames.clear();
		for (final LispStruct current : newNicknames) {
			final StringStruct newNickname = StringStruct.fromDesignator(current);
			final String nickname = newNickname.toJavaString();
			if (GlobalPackageStruct.ALL_PACKAGES.containsKey(nickname)) {
				throw new PackageErrorException("A package named " + nickname + " already exists.", this);
			}
			nicknames.add(nickname);
			GlobalPackageStruct.ALL_PACKAGES.put(nickname, this);
		}

		return this;
	}

	@Override
	public BooleanStruct deletePackage() {
		if (name == null) {
			return NILStruct.INSTANCE;
		}

		for (final PackageStruct usedPackage : useList) {
			unUsePackage(usedPackage);
		}
		for (final PackageStruct usedByPackage : usedByList) {
			usedByPackage.unUsePackage(this);
		}

		GlobalPackageStruct.ALL_PACKAGES.remove(name);
		nicknames.forEach(GlobalPackageStruct.ALL_PACKAGES::remove);

		makeSymbolsUninterned(internalSymbols);
		makeSymbolsUninterned(externalSymbols);

		nicknames.clear();

		return TStruct.INSTANCE;
	}

	/**
	 * Make the symbols in the provided {@code symbolMap} uninterned from the current package and clears out the symbol
	 * map.
	 *
	 * @param symbolMap
	 * 		the map of symbols to unintern and clear
	 */
	private void makeSymbolsUninterned(final Map<String, SymbolStruct> symbolMap) {
		for (final SymbolStruct symbol : symbolMap.values()) {
			if (symbol.symbolPackage() == this) {
				symbol.setSymbolPackage(null);
			}
		}
		symbolMap.clear();
	}

	@Override
	public void usePackage(final PackageStruct packageToUse) {
		if (!useList.contains(packageToUse)) {
			// "USE-PACKAGE checks for name conflicts between the newly
			// imported symbols and those already accessible in package."
			for (final SymbolStruct symbol : packageToUse.getExternalSymbols()) {
				final String symName = symbol.getName();

				final Optional<SymbolStruct> existing = findAccessibleSymbol(symName);
				if (existing.isPresent() && (existing.get() != symbol)) {
					if (shadowingSymbols.get(symName) == null) {
						throw getAccessibilityException(symName, false);
					}
				}
			}
			useList.add(packageToUse);
			// Add this package to the used-by list of pkg.
			packageToUse.getUsedByList().add(this);
		}
	}

	@Override
	public BooleanStruct usePackage(final ListStruct packagesToUse) {
		for (final LispStruct current : packagesToUse) {
			final PackageStruct pkg = PackageStruct.fromDesignator(current);
			usePackage(pkg);
		}
		return TStruct.INSTANCE;
	}

	@Override
	public void unUsePackage(final PackageStruct packageToUnUse) {
		// NOTE: We will leave the shadows in the shadowing list. This is due to the fact that we would have to search
		// through ALL used packages to make sure that there aren't any other inherited symbols that the symbol names
		// are shadowing. That's just overkill, when keeping them in the shadowing list won't affect anything.
		useList.remove(packageToUnUse);
		packageToUnUse.getUsedByList().remove(this);
	}

	@Override
	public BooleanStruct unUsePackage(final ListStruct packagesToUnUse) {
		for (final LispStruct current : packagesToUnUse) {
			final PackageStruct pkg = PackageStruct.fromDesignator(current);
			unUsePackage(pkg);
		}
		return TStruct.INSTANCE;
	}

	@Override
	public PackageSymbolStruct findSymbol(final String symbolName) {
		final Optional<PackageSymbolStruct> symbol = findSymbolInternal(symbolName);
		return symbol.orElseGet(() -> new PackageSymbolStruct(NILStruct.INSTANCE, NILStruct.INSTANCE));
	}

	/**
	 * Locates a {@link PackageSymbolStruct} with the provided {@code symbolName} accessible within the package from
	 * either 'external', 'internal', or 'inherited' symbols.
	 *
	 * @param symbolName
	 * 		the name of the symbol to locate
	 *
	 * @return a possible {@link PackageSymbolStruct} for the located symbol
	 */
	private Optional<PackageSymbolStruct> findSymbolInternal(final String symbolName) {
		SymbolStruct symbol = externalSymbols.get(symbolName);
		if (symbol != null) {
			return Optional.of(new PackageSymbolStruct(symbol, CommonLispSymbols.EXTERNAL_KEYWORD));
		}

		symbol = internalSymbols.get(symbolName);
		if (symbol != null) {
			return Optional.of(new PackageSymbolStruct(symbol, CommonLispSymbols.INTERNAL_KEYWORD));
		}

		for (final PackageStruct usedPackage : useList) {
			final Optional<SymbolStruct> externalSymbol = usedPackage.findExternalSymbol(symbolName);
			if (externalSymbol.isPresent()) {
				return Optional.of(new PackageSymbolStruct(
						externalSymbol.get(), CommonLispSymbols.INHERITED_KEYWORD
				));
			}
		}
		return Optional.empty();
	}

	@Override
	public void importSymbol(final SymbolStruct symbol) {
		final Optional<PackageStruct> symbolPackage = symbol.getSymbolPackage();
		if (symbolPackage.isPresent() && (symbolPackage.get() == this)) {
			return;
		}
		final String symbolName = symbol.getName();

		verifySymbolAccessibility(symbol, symbolName, false);

		internalSymbols.put(symbolName, symbol);
		if (symbolPackage.isEmpty()) {
			symbol.setSymbolPackage(this);
		}
	}

	@Override
	public BooleanStruct importSymbols(final ListStruct symbols) {
		for (final LispStruct current : symbols) {
			final SymbolStruct symbol = checkSymbol(current);
			importSymbol(symbol);
		}
		return TStruct.INSTANCE;
	}

	@Override
	public void shadowingImport(final SymbolStruct symbol) {
		final String symbolName = symbol.getName();

		SymbolStruct sym = externalSymbols.get(symbolName);
		if (sym == null) {
			sym = internalSymbols.get(symbolName);
		}

		// if a different symbol with the same name is accessible,
		// [..] which implies that it must be uninterned if it was present
		if ((sym != null) && (sym != symbol)) {
			shadowingSymbols.remove(symbolName);
			unintern(sym);
		}

		if ((sym == null) || (sym != symbol)) {
			// there was no symbol, or we just uninterned it another one, so intern the new one
			internalSymbols.put(symbolName, symbol);
		}

		shadowingSymbols.put(symbolName, symbol);
	}

	@Override
	public BooleanStruct shadowingImport(final ListStruct symbols) {
		for (final LispStruct current : symbols) {
			final SymbolStruct symbol = checkSymbol(current);
			shadowingImport(symbol);
		}
		return TStruct.INSTANCE;
	}

	@Override
	public void export(final SymbolStruct symbol) {
		final String symbolName = symbol.getName();
		boolean added = false;
		if (symbol.symbolPackage() != this) {
			verifySymbolAccessibility(symbol, symbolName, true);
			internalSymbols.put(symbolName, symbol);
			added = true;
		}

		if (added || (internalSymbols.get(symbolName) == symbol)) {
			for (final PackageStruct usedByPackage : usedByList) {
				final Optional<SymbolStruct> accessibleSymbol = usedByPackage.findAccessibleSymbol(symbolName);
				if (accessibleSymbol.isPresent() && (accessibleSymbol.get() != symbol)) {
					final Optional<SymbolStruct> shadowedSymbol = usedByPackage.findShadowedSymbol(symbolName);
					if (shadowedSymbol.isPresent() && (shadowedSymbol.get() != accessibleSymbol.get())) {
						final String symName = accessibleSymbol.get().getName();
						final String message = "The symbol " + symName + " is already accessible in package " + usedByPackage + '.';
						throw new PackageErrorException(message, this);
					}
				}
			}

			// No conflicts.
			internalSymbols.remove(symbolName);
			externalSymbols.put(symbolName, symbol);
			return;
		}

		// Symbol is already exported; there's nothing to do.
		if (externalSymbols.get(symbolName) == symbol) {
			return;
		}

		throw getAccessibilityException(symbolName, true);
	}

	@Override
	public BooleanStruct export(final ListStruct symbols) {
		for (final LispStruct current : symbols) {
			final SymbolStruct symbol = checkSymbol(current);
			export(symbol);
		}
		return TStruct.INSTANCE;
	}

	@Override
	public void unexport(final SymbolStruct symbol) {
		final String symbolName = symbol.getName();

		if (externalSymbols.get(symbolName) == symbol) {
			externalSymbols.remove(symbolName);
			internalSymbols.put(symbolName, symbol);
			return;
		}
		verifySymbolAccessibility(symbol, symbolName, true);
	}

	@Override
	public BooleanStruct unexport(final ListStruct symbols) {
		for (final LispStruct current : symbols) {
			final SymbolStruct symbol = checkSymbol(current);
			unexport(symbol);
		}
		return TStruct.INSTANCE;
	}

	@Override
	public void shadow(final StringStruct symbolName) {
		final String symbolNameString = symbolName.toJavaString();
		SymbolStruct symbol = externalSymbols.get(symbolNameString);
		if (symbol != null) {
			shadowingSymbols.put(symbolNameString, symbol);
			return;
		}

		symbol = internalSymbols.get(symbolNameString);
		if (symbol != null) {
			shadowingSymbols.put(symbolNameString, symbol);
			return;
		}

		if (shadowingSymbols.get(symbolNameString) != null) {
			return;
		}

		symbol = internNewSymbol(symbolNameString);
		shadowingSymbols.put(symbolNameString, symbol);
	}

	@Override
	public BooleanStruct shadow(final ListStruct symbolNames) {
		for (final LispStruct current : symbolNames) {
			final StringStruct symbolName = StringStruct.fromDesignator(current);
			shadow(symbolName);
		}
		return TStruct.INSTANCE;
	}

	@Override
	public PackageSymbolStruct intern(final String symbolName) {
		final Optional<PackageSymbolStruct> symbol = findSymbolInternal(symbolName);
		if (symbol.isPresent()) {
			return symbol.get();
		}

		final SymbolStruct newSymbol = internNewSymbol(symbolName);
		return new PackageSymbolStruct(newSymbol, NILStruct.INSTANCE);
	}

	/**
	 * Interns a newly created {@link SymbolStruct} with this {@link PackageStruct} as its package.
	 * <p>
	 * This method can be overridden by subtypes.
	 *
	 * @param symbolName
	 * 		the name of the {@link SymbolStruct} to create and intern
	 *
	 * @return the newly created {@link SymbolStruct}
	 */
	protected SymbolStruct internNewSymbol(final String symbolName) {
		final SymbolStruct symbol = SymbolStruct.toLispSymbol(symbolName);
		symbol.setSymbolPackage(this);

		verifySymbolAccessibility(symbol, symbolName, false);
		internalSymbols.put(symbolName, symbol);
		return symbol;
	}

	@Override
	public BooleanStruct unintern(final SymbolStruct symbol) {
		final String symbolName = symbol.getName();

		final boolean isSymbolShadowed = shadowingSymbols.get(symbolName) == symbol;
		if (isSymbolShadowed) {
			// Check for conflicts that might be exposed in used package list
			// if we remove the shadowing symbol.
			validateShadowedSymbol(symbol);
		}

		// Reaching here, it's OK to remove the symbol.
		boolean isSymbolUninterned = true;
		if (externalSymbols.get(symbolName) == symbol) {
			externalSymbols.remove(symbolName);
			isSymbolUninterned = false;
		}
		if (internalSymbols.get(symbolName) == symbol) {
			internalSymbols.remove(symbolName);
			isSymbolUninterned = false;
		}

		if (isSymbolUninterned) {
			return NILStruct.INSTANCE;
		}

		if (isSymbolShadowed) {
			shadowingSymbols.remove(symbolName);
		}

		if (symbol.symbolPackage() == this) {
			symbol.setSymbolPackage(null);
		}
		return TStruct.INSTANCE;
	}

	/**
	 * Verifies the provided {@link SymbolStruct} is valid to be uninterned from the package, meaning exist multiple
	 * external symbols in the {@link #useList} packages with the same name as the provided {@link SymbolStruct}, a
	 * {@link PackageErrorException} will be thrown noting the symbol conflicts.
	 *
	 * @param symbol
	 * 		the symbol the verify can be uninterned from the packag e
	 */
	private void validateShadowedSymbol(final SymbolStruct symbol) {
		final String symbolName = symbol.getName();

		SymbolStruct sym = null;
		for (final PackageStruct usedPackage : useList) {
			final Optional<SymbolStruct> externalSymbol = usedPackage.findExternalSymbol(symbolName);
			if (externalSymbol.isPresent()) {
				final SymbolStruct extSymbol = externalSymbol.get();
				if (sym == null) {
					sym = extSymbol;
				} else if (sym != extSymbol) {
					final String message = "Uninterning the symbol " + symbol +
							" causes a name conflict between " + sym + " and " + extSymbol;
					throw new PackageErrorException(message, this);
				}
			}
		}
	}

	/**
	 * Checks the provided structure is a {@link SymbolStruct} and returns it. If the value is not a {@link
	 * SymbolStruct} a {@link TypeErrorException} is thrown.
	 *
	 * @param struct
	 * 		the structure to check
	 *
	 * @return the structure as a {@link SymbolStruct}
	 */
	private static SymbolStruct checkSymbol(final LispStruct struct) {
		if (struct instanceof SymbolStruct) {
			return (SymbolStruct) struct;
		} else {
			throw new TypeErrorException("Type cannot be converted to Symbol: " + struct);
		}
	}

	/**
	 * Verifies a {@link SymbolStruct} is present in this package with the provided {@code symbolName} and is the same
	 * instance as the provided {@link SymbolStruct} in the package. If this is the case, a {@link
	 * PackageErrorException} will be thrown.
	 *
	 * @param symbol
	 * 		the {@link SymbolStruct} to verify matches an existing accessible symbol
	 * @param symbolName
	 * 		name of the accessible {@link SymbolStruct} to find
	 * @param shouldBeAccessible
	 * 		whether or not the {@link SymbolStruct} should be accessible or not
	 */
	private void verifySymbolAccessibility(final SymbolStruct symbol, final String symbolName,
	                                       final boolean shouldBeAccessible) {
		final Optional<SymbolStruct> accessibleSymbol = findAccessibleSymbol(symbolName);
		if (accessibleSymbol.isPresent() && (accessibleSymbol.get() != symbol)) {
			throw getAccessibilityException(symbolName, shouldBeAccessible);
		}
	}

	/**
	 * Returns a new {@link PackageErrorException} with a properly crafted message related to the accessibility of a
	 * symbol in the package with the provided {@code symbolName}.
	 *
	 * @param symbolName
	 * 		the name of the symbol that has accessibility issues
	 * @param shouldBeAccessible
	 * 		whether or not the symbol should have been accessible or not
	 *
	 * @return new {@link PackageErrorException} with a properly crafted message
	 */
	private PackageErrorException getAccessibilityException(final String symbolName, final boolean shouldBeAccessible) {
		final String accessibleString = shouldBeAccessible ? "not" : "already";
		final String message = "The symbol " + symbolName + " is " + accessibleString + " accessible in package " + name + '.';
		return new PackageErrorException(message, this);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.PACKAGE;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.PACKAGE;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.PACKAGE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.PACKAGE) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	@Override
	public String toString() {
		return "#<PACKAGE \"" + name + "\">";
	}
}
